/**
 *
 * Teleview worker 
 *
 */

function initialState() {
    return {
        teleview_id: undefined,
        renderer_id: undefined,

        updateTopic: undefined,

        keyframe: undefined,
        keyframe_sn: undefined,

        current_frame: undefined,
        current_frame_sn: undefined,

        max_time: undefined,
        min_time: undefined,

        encoder: undefined,
        decoder: undefined,

        pending_server_status_request: false,
        stop: false,

        page_state: "active",
        hidden_start_time: undefined,
        need_server_side_check: false
    };
}

const model = initialState();
const view = {};
const state = {};
const actions = {};


/**
 * Model
 */

model.present = function(proposal) {
    if(proposal.is_start) {
        const arg = proposal.arg;

        model.pickle = arg.pickle;

        model.teleview_id = arg.teleview_id;
        model.renderer_id = arg.renderer_id;

        model.updateTopic = cotonic.mqtt.fill("model/ui/update/+uiId", {uiId: arg.uiId});

        model.encoder = new TextEncoder();
        model.decoder = new TextDecoder();

        model.keyframe = model.encoder.encode(arg.keyframe);
        model.keyframe_sn = arg.keyframe_sn;

        model.current_frame = (arg.current_frame === undefined) ? undefined : model.encoder.encode(arg.current_frame);
        model.current_frame_sn = arg.current_frame_sn;

        model.max_time = arg.max_time;
        model.min_time = arg.min_time;

        /* */
        self.publish(
            cotonic.mqtt.fill("model/ui/insert/+uiId", arg),
            {
                initialData: undefined,
                inner: true,
                priority: 10
            }
        );

        self.publish("model/teleview/" + model.televiewId + "/event/started", true);

        self.subscribe(televiewEventTopic(model), actions.televiewEvent);
        self.subscribe(rendererEventTopic(model), actions.rendererEvent);

        self.subscribe("model/lifecycle/event/state", actions.lifecycleEvent);
    }

    if(proposal.is_update) {
        switch(proposal.type) {
            case "keyframe":
                if(model.keyframe_sn === undefined || proposal.update.keyframe_sn > model.keyframe_sn) {
                    model.keyframe = model.current_frame = model.encoder.encode(proposal.update.frame);
                    model.keyframe_sn = model.current_frame_sn = proposal.update.keyframe_sn;

                    self.publish(model.updateTopic, model.decoder.decode(model.current_frame));
                } else {
                    console.log("Keyframe in update is older than current state.");
                }

                break;
            case "incremental": // Patch against the current frame.
                if(!model.current_frame) {
                    console.log("waiting for current frame");
                } else {
                    if(model.current_frame_sn + 1=== proposal.update.current_frame_sn) {
                        model.current_frame = applyPatch(model.current_frame, proposal.update, model.encoder);
                        model.current_frame_sn = proposal.update.current_frame_sn;

                        self.publish(model.updateTopic, model.decoder.decode(model.current_frame));
                    } else {
                        console.log("Incremental patch does not match current frame sn", model.current_frame_sn, proposal.update.current_frame_sn);
                    }
                }

                break;
            case "cumulative": // Patch against the keyframe
                if(!model.keyframe) {
                    console.log("waiting for keyframe");
                } else {
                    if(model.keyframe_sn === proposal.update.keyframe_sn) {
                        model.current_frame = applyPatch(model.keyframe, proposal.update, model.encoder);

                        // Update the current frame sn when it is available. 
                        if(proposal.update.current_frame_sn !== undefined) {
                           model.current_frame_sn = proposal.update.current_frame_sn;
                        }

                        self.publish(model.updateTopic, model.decoder.decode(model.current_frame));
                    } else {
                        console.log("Cumulative patch does not match keyframe sn");
                    }
                }

                break;
            default:
                throw Error("Unexpected update", proposal.type);
        }
    }

    if(proposal.is_reset) {
        model.keyframe  = undefined;
        model.keyframe_sn = 0;

        model.current_frame = undefined;
        model.current_frame_sn = 0;
    }

    if(proposal.is_still_watching && model.updateTopic) {
        self.publish(
            cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", model),
            {});
    }

    if(proposal.is_stop && model.updateTopic) {
        self.publish(model.updateTopic, "<p>Teleview stopped</p>");

        // [TODO] maybe stop the worker.

        self.unsubscribe(televiewEventTopic(model), actions.televiewEvent);
        self.unsubscribe(rendererEventTopic(model), actions.rendererEvent);
        
        self.publish("model/teleview/" + model.televiewId + "/event/stopped", true);

        model = initialState();
    }

    // Check if the server side still exists
    if(proposal.is_ensure_server_side && !model.pending_server_status_request && model.updateTopic) {
        self.call(
            cotonic.mqtt.fill("bridge/origin/model/teleview/get/+teleview_id/state/+renderer_id", model),
            model.pickle).then(
                actions.handleEnsureStatus,
                actions.errorEnsureStatus
            );

        model.need_server_side_check = false;
        model.pending_server_status_request = true;
    }

    // Got a result from the server status. 
    if(proposal.is_ensure_server_status && model.pending_server_status_request) {
        model.pending_server_status_request = false;

        const arg = proposal.arg;

        model.keyframe = model.encoder.encode(arg.keyframe);
        model.keyframe_sn = arg.keyframe_sn;

        model.current_frame = (arg.current_frame === undefined) ? undefined : model.encoder.encode(arg.current_frame);
        model.current_frame_sn = arg.current_frame_sn  

        model.max_time = arg.max_time;
        model.min_time = arg.min_time;

        self.publish(model.updateTopic, model.decoder.decode(model.current_frame));
    }

    if(proposal.is_ensure_server_status_error && model.pending_server_status_request) {
        model.pending_server_status_request = false;

        model.stop = true;
    }

    if(proposal.is_lifecycle_event) {
        if(model.page_state === "passive" && proposal.state === "hidden") {
            model.hidden_start_time = Date.now(); 
        }

        if(model.page_state === "hidden" && proposal.state === "passive") {
            const now = Date.now();

            if(model.hidden_start_time !== undefined && ((now - model.hidden_start_time) > 300000)) {
                model.need_server_side_check = true;
            }

            model.hidden_start_time = undefined;
        }

        model.page_state = proposal.state;
    }

    state.render(model);
}

/**
 * View
 */

view.display = function(representation) {
}

/**
 * State
 */

state.view = view;

state.render = function(model) {
    state.view.display(state.representation(model));
    state.nextAction(model) ;
}

state.representation = function(model) {
}

state.nextAction = function(model) {
    if(model.stop && model.updateTopic) {
        actions.stop();
    }

    if(model.need_server_side_check) {
        actions.ensureServerSide();
    }
}

/**
 * Actions
 */

actions.televiewEvent = function(m, a) {
    switch(a.evt_type) {
        case "stopped": actions.stop();
    }
}

actions.rendererEvent = function(m, a) {
    switch(a.evt_type) {
        case "update":
            actions.update(a.args[0], m.payload);
            break;
        case "reset":
            actions.reset();
            break;
        case "still_watching":
            actions.still_watching();
            break;
    }
}

actions.update = function (type, update) {
    model.present({
        is_update: true,
        type: type,
        update: update
    });
}

actions.reset = function() {
    model.present({is_reset: true});
}

actions.stop = function() {
    model.present({is_stop: true})
}

actions.still_watching = function() {
    model.present({is_still_watching: true});
}

actions.start = function(args) {
    model.present( {is_start: true, arg: args[0] } );
}

actions.ensureServerSide = function() {
    model.present({is_ensure_server_side: true});
}

actions.handleEnsureStatus = function(m, a) {
    if(m.payload && m.payload.status === "ok") {
        model.present({
            is_ensure_server_status: true,
            arg: m.payload.result
        });
    } else {
        model.present({
            is_ensure_server_status_error: true,
            arg: m.payload
        });
    }
   }

actions.errorEnsureStatus = function(m, a) {
    model.present({
        is_ensure_server_status_error: true,
        arg: m.payload
    });
}

actions.lifecycleEvent = function(m, a) {
    model.present({
        is_lifecycle_event: true,
        state: m.payload
    });
}

/**
 * Helpers
 */

function televiewEventTopic(model) {
    return "bridge/origin/model/teleview/event/" + model.teleview_id + "/+evt_type";
}

function rendererEventTopic(model) {
    return televiewEventTopic(model) + "/" + model.renderer_id + "/#args";
}

function applyPatch(source, update, encoder) {
    if(update.patch.length === 0)
        return source;

    const buffer = new ArrayBuffer(update.result_size);
    const array = new Uint8Array(buffer);
    const src = source;

    function copyInto(dst, dst_offset, src, src_offset, length) {
        src_offset = (src_offset === undefined) ? 0 : src_offset;
        length = (length === undefined) ? src.length : length;

        for(let i = 0; i < length; i++) {
            dst[dst_offset+i] = src[src_offset+i];
        }
    }

    let src_idx = 0;
    let dst_idx = 0;

    for(let i = 0, l = update.patch.length; i < l; i += 2) {
        const patch = update.patch[i];
        const v = update.patch[i+1];

        switch(patch) {
            case "c": // copy src into destination buffer.
                copyInto(array, dst_idx, src, src_idx, v);
                src_idx += v;
                dst_idx += v;
                break;
            case "s": // skip src
                src_idx += v;
                break;
            case "i": // insert new string into destination buffer.
                const data = encoder.encode(v)
                copyInto(array, dst_idx, data);
                dst_idx += data.length;
                break;
            default:
                throw Error("Unexpected patch");
        }
    }

    return array;
}


/**
 * Worker startup
 */

self.connect({
    depends: [ "bridge/origin", "model/lifecycle" ],
}).then(actions.start)
