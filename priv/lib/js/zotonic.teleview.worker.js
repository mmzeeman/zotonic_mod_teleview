/**
 *
 * Teleview worker 
 *
 */


const model = {
    id: undefined,
    teleview_id: undefined,
    renderer_id: undefined,

    updateTopic: undefined,

    keyframe: undefined,
    keyframe_sn: undefined,

    current_frame: undefined,
    current_frame_sn: undefined,
    isCurrentFrameRequested: false,

    queuedCumulativePatch: undefined,
    incrementalPatchQueue: [],

    max_time: undefined,
    min_time: undefined,

    stop: false,

    page_state: "active",

    hidden_start_time: undefined,
    need_new_current_frame: false
};


const view = {};
const state = {};
const actions = {};


/**
 * Model
 */

model.present = function(proposal) {

    /*
     * Start
     */

    if(proposal.is_start) {
        const arg = proposal.arg;

        model.id = arg.id;
        model.teleview_id = arg.teleview_id;
        model.renderer_id = arg.renderer_id;

        model.updateTopic = cotonic.mqtt.fill("model/ui/update/+id", model);

        model.keyframe = undefined;
        model.keyframe_sn = undefined;

        model.current_frame = undefined;
        model.current_frame_sn = undefined;
        model.isCurrentFrameRequested = false;

        model.queuedCumulativePatch = undefined; 
        model.incrementalPatchQueue = [];

        model.min_time = (arg.keyframe_min_time === undefined)?0:arg.keyframe_min_time;
        model.max_time = (arg.keyframe_max_time === undefined)?"infinite":arg.keyframe_max_time;

        self.publish(
            cotonic.mqtt.fill("model/ui/insert/+id", model),
            {
                initialData: arg.initial_content,
                inner: true,
                priority: 10
            }
        );
        self.subscribe(televiewEventTopic(model), actions.televiewEvent);
        self.subscribe(rendererEventTopic(model), actions.rendererEvent);
        self.subscribe("model/lifecycle/event/state", actions.lifecycleEvent);

        // We are started.
        self.publish(cotonic.mqtt.fill("model/teleview/+televiewId/event/started", model), true);
    }

    /*
     * Update
     */

    if(proposal.is_update) {
        switch(proposal.type) {
            case "keyframe":
                model.isCurrentFrameRequested = false;

                if(model.keyframe_sn === undefined || proposal.update.keyframe_sn > model.keyframe_sn) {
                    model.keyframe = model.current_frame = toUTF8(proposal.update.frame);
                    model.keyframe_sn = model.current_frame_sn = proposal.update.keyframe_sn;

                    if(model.queuedCumulativePatch) {
                        model.current_frame = applyPatch(model.current_frame, model.queuedCumulativePatch);
                        model.current_frame_sn = model.queuedCumulativePatch.current_frame_sn;
                    }

                    model.incrementalPatchQueue = [];
                    model.queuedCumulativePatch = undefined;
                } else {
                    // Ingore this frame, the current frame is newer.
                    console.log("Keyframe in update is older than current state.");
                }

                break;
            case "current_frame":
                model.isCurrentFrameRequested = false;

                if(model.current_frame_sn === undefined || proposal.update.current_frame_sn > model.current_frame_sn) {
                    model.current_frame = toUTF8(proposal.update.current_frame);
                    model.current_frame_sn = proposal.update.current_frame_sn;

                    while(model.incrementalPatchQueue.length) {
                        const p = model.incrementalPatchQueue.shift();

                        if(model.current_frame_sn + 1 !== p.current_frame_sn) {
                            continue; // skip
                        }

                        model.current_frame = applyPatch(model.current_frame, p);
                        model.current_frame_sn = p.current_frame_sn;
                    }
                }

                break;
            case "cumulative": // Patch against the keyframe
                if(model.keyframe && model.keyframe_sn === proposal.update.keyframe_sn) {
                    model.current_frame = applyPatch(model.keyframe, proposal.update);

                    // Update the current frame sn when it is available. 
                    if(proposal.update.current_frame_sn !== undefined) {
                        model.current_frame_sn = proposal.update.current_frame_sn;
                    }
                } else {
                    // When a keyframe arrives, it could be that it is possible to use this patch 
                    model.queuedCumulativePatch = proposal.update;
                }

                break;

            case "incremental": // Patch against the current frame.
                if(model.current_frame && model.current_frame_sn + 1 === proposal.update.current_frame_sn) {
                    model.current_frame = applyPatch(model.current_frame, proposal.update);
                    model.current_frame_sn = proposal.update.current_frame_sn;
                } else {
                    model.requestCurrentFrame();
                    model.incrementalPatchQueue.push(proposal.update);
                }

                break;

            default:
                throw Error("Unexpected update", proposal.type);
        }
    }

    /*
     * Reset
     */

    if(proposal.is_reset) {
        model.keyframe  = undefined;
        model.keyframe_sn = 0;

        model.current_frame = undefined;
        model.current_frame_sn = 0;
        model.isCurrentFrameRequested = false;

        model.queuedCumulativePatch = undefined; 
        model.incrementalPatchQueue = [];
        model.need_new_current_frame = false;
    }

    /*
     * Request Current Frame
     */
    if(proposal.is_request_current_frame) {
        model.requestCurrentFrame();
    }

    /*
     * Still watching? 
     */

    if(proposal.is_still_watching && model.updateTopic && state.isPageVisible(model)) {
        self.publish(
            cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", model),
            {});
    }

    /*
     * Stop
     */

    if(proposal.is_stop && model.updateTopic) {
        self.publish(cotonic.mqtt.fill("model/teleview/+televiewId/event/stopped", model), true);
    }

    /*
     * Page lifecycle event
     */
    if(proposal.is_lifecycle_event) {
        if(model.page_state === "passive" && proposal.state === "hidden") {
            model.hidden_start_time = Date.now(); 
        }

        // Page moved from hidden to passive.
        if(model.page_state === "hidden" && proposal.state === "passive") {
            const now = Date.now();

            if(model.hidden_start_time !== undefined && ((now - model.hidden_start_time) > 5000)) {
                model.need_new_current_frame = true;
            }

            model.hidden_start_time = undefined;

            // Keep the teleview alive, we moved from hidden to passive.
            self.publish(
                cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", model),
                {});
        }

        model.page_state = proposal.state;
    }

    /*
     * There was a current frame request error, and we need a new frame.
     */
    if(proposal.is_current_frame_request_error) {
        model.isCurrentFrameRequested = false;
        model.need_new_current_frame = true;
    }

    state.render(model);
}

model.requestCurrentFrame = function() {
    if(model.isCurrentFrameRequested)
        return;

    model.isCurrentFrameRequested = true;

    self.call(cotonic.mqtt.fill("bridge/origin/model/teleview/get/+teleview_id/current_frame/+renderer_id", model),
              undefined,
              {qos: 1})
        .then(actions.currentFrameResponse)
        .catch(actions.currentFrameRequestError);
}


/**
 * View
 */

view.display = function(representation) {
    if(!representation)
        return;

    self.publish(model.updateTopic, representation);
}

/**
 * State
 */

state.view = view;

state.render = function(model) {
    state.view.display(state.representation(model));
    state.nextAction(model) ;
}

state.isStarted = function(model) {
    return !!model.updateTopic;
}

state.hasCurrentFrame = function(model) {
    return !!model.current_frame;
}

state.isPageVisible = function(model) {
    return (model.page_state === "active" || model.page_state === "passive");
}

state.representation = function(model) {
    if(!state.isStarted(model)) return;
    if(!state.hasCurrentFrame(model)) return;
    if(!state.isPageVisible(model)) return;

    return fromUTF8(model.current_frame);
}

state.nextAction = function(model) {
    if(model.stop && model.updateTopic) {
        actions.stop();
    }

    if(model.need_new_current_frame) {
        actions.requestCurrentFrame(true);
    }
}

/**
 * Actions
 */

actions.televiewEvent = function(m, a) {
    switch(a.evt_type) {
        case "stopped":
            // The server side was stopped.
            actions.stop(m.payload.reason);
            break;
        case "started":
            // The server-side teleview just (re)stated, request
            // the current frame
            actions.requestCurrentFrame(true);
            break;
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
        case "down":
            actions.rendererDown(m.payload.reason);
            break;
        default:
            console.log("Unknown renderer event", a.evt_type);
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


actions.requestCurrentFrame = function(withReset) {
    model.present({
        is_request_current_frame: true,
        is_reset: true
    });
}

actions.stop = function(reason) {
    model.present({is_stop: true, reason: reason})
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

actions.keyframeResponse = function(m) {
    const p = m.payload;
    if(p.status === "ok") {
        model.present({
            type: "keyframe",
            is_update: true,
            update: p.result
        });
    }
}

actions.currentFrameResponse = function(m) {
    const p = m.payload;
    if(p.status === "ok") {
        if(p.result.state === "ok") {
            model.present({
                type: "current_frame",
                is_update: true,
                update: p.result
            });
        } else if(p.result.state === "restarting") {
            // The teleview is going to be restarted.
            actions.reset();
        } else {
            console.log("Unknown current frame response", m);
        }
    }
}

actions.currentFrameRequestError = function(m) {
    model.present({is_current_frame_request_error: true});
}

actions.rendererDown = function(reason) {
    // Communicate this to the surrounding div, and maybe sent a notification.
    console.log("Renderer down");
}

/**
 * Helpers
 */

const toUTF8 = (function() { const e = new TextEncoder(); return e.encode.bind(e); })();

const fromUTF8 = (function() { const d = new TextDecoder(); return d.decode.bind(d); })();

function televiewEventTopic(model) {
    return `bridge/origin/model/teleview/event/${ model.teleview_id }/+evt_type`;
}

function rendererEventTopic(model) {
    return `${ televiewEventTopic(model) }/${ model.renderer_id }/#args`;
}

function applyPatch(source, update) {
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
                const data = toUTF8(v)
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
