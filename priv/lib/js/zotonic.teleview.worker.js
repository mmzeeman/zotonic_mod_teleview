/**
 *
 * Teleview worker 
 *
 */

const model = {
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
    decoder: undefined
};

const view = {};
const state = {};
const actions = {};


/**
 * Model
 */

model.present = function(proposal) {
    if(proposal.is_start) {
        const arg = proposal.arg;

        model.teleview_id = arg.teleview_id;
        model.renderer_id = arg.renderer_id;

        model.updateTopic = cotonic.mqtt.fill("model/ui/update/+uiId", {uiId: arg.uiId});

        model.encoder = new TextEncoder();
        model.decoder = new TextDecoder();

        model.keyframe = model.encoder.encode(arg.keyframe);
        model.keyframe_sn = arg.keyframe_sn;

        model.current_frame = (model.current_frame===undefined) ? undefined : model.encoder.encode(model.current_frame);
        model.current_frame_sn =  model.current_frame_sn;

        model.max_time = arg.max_time,
        model.min_time = arg.min_time,

        /* */
        self.publish(
            cotonic.mqtt.fill("model/ui/insert/+uiId", arg),
            {
                initialData: undefined,
                inner: true,
                priority: 10
            }
        );

        const televiewEventTopic = "bridge/origin/model/teleview/event/" + model.teleview_id + "/+evt_type";
        const rendererEventTopic = televiewEventTopic + "/" + model.renderer_id + "/#args";

        self.subscribe(televiewEventTopic, actions.televiewEvent);
        self.subscribe(rendererEventTopic, actions.renderEvent);
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
                    if(model.current_frame_sn === proposal.update.current_frame_sn) {
                        model.current_frame = applyPatch(model.current_frame, proposal.update, model.encoder);
                        model.current_frame_sn = proposal.update.current_frame_sn;

                        self.publish(model.updateTopic, model.decoder.decode(model.current_frame));
                    } else {
                        console.log("Incremental patch does not match current frame sn");
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

    if(proposal.is_still_watching && model.uiUpdateTopic) {
        self.publish(
            cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", model),
            {});
    }

    if(proposal.is_stop && model.uiUpdateTopic) {
        self.publish(model.uiUpdateTopic, "<p>Teleview stopped</p>");

        // [TODO] I think the worker can be stopped now.
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
}

/**
 * Actions
 */

actions.televiewEvent = function(m, a) {
    switch(a.evt_type) {
        case "stopped": actions.stop();
    }
}

actions.renderEvent = function(m, a) {
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


/**
 * Helpers
 */


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
