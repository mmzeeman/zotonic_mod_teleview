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

    sts: undefined, 

    keyframe: undefined,
    keyframe_sn: undefined,

    current_frame: undefined,
    current_frame_sn: undefined,

    queuedCumulativePatch: undefined,
    incrementalPatchQueue: [],

    isCurrentFrameRequested: false,

    max_time: undefined,
    min_time: undefined,

    pickle: undefined,

    stop: false,
    page_state: "active",
    hidden_start_time: undefined
};

const view = {};
const state = {};
const actions = {};

/**
 * Model
 */

model.present = (proposal) => {

    /*
     * Start
     */
    if(proposal.is_start) {
        const arg = proposal.arg;

        model.id = arg.id;
        model.sts = arg.renderer_sts;
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

        model.pickle = arg.pickle;

        self.publish(cotonic.mqtt.fill("model/ui/insert/+id", model), { inner: true, priority: 10 });

        self.subscribe(rendererEventTopic(model), actions.rendererEvent);
        self.subscribe("model/lifecycle/event/state", actions.lifecycleEvent);

        // We are started.
        self.publish(cotonic.mqtt.fill("model/televiewClient/+televiewId/event/started", model), true);
    }

    /*
     * Reset
     */
    if(proposal.is_reset || (proposal.is_update && proposal.update.sts > model.sts)) {
        model.sts = undefined;
        model.keyframe  = undefined;
        model.keyframe_sn = 0;

        model.current_frame = undefined;
        model.current_frame_sn = 0;
        model.isCurrentFrameRequested = false;

        model.queuedCumulativePatch = undefined; 
        model.incrementalPatchQueue = [];
    }

    /*
     * Update
     */
    if(proposal.is_update) {
        if(model.sts === undefined || proposal.update.sts === model.sts) {
            switch(proposal.type) {
                case "keyframe":
                    if(model.keyframe_sn === undefined || proposal.update.keyframe_sn > model.keyframe_sn) {
                        if(model.sts === undefined) {
                            model.sts = proposal.update.sts;
                        }
                        model.keyframe = toUTF8(proposal.update.frame);
                        model.keyframe_sn = proposal.update.keyframe_sn;
                    
                        // Only push a new current frame when we have on. If there is no current frame yet,
                        // the content on the screen during rendering could be newer than the current keyframe.
                        if(model.current_frame) {
                            model.current_frame = model.keyframe;
                            model.current_frame_sn = model.keyframe_sn;
                        }

                        if(model.queuedCumulativePatch) {
                            model.current_frame = applyPatch(model.current_frame, model.queuedCumulativePatch);
                            model.current_frame_sn = model.queuedCumulativePatch.current_frame_sn;
                        }

                        model.incrementalPatchQueue = [];
                        model.queuedCumulativePatch = undefined;

                    } else {
                        console.info("Teleview: Ignore keyframe, it is older or has same serial number as current keyframe.",
                            {id: model.id,
                                keyframe_sn: model.keyframe_sn,
                                update_keyframe_sn: proposal.update.keyframe_sn});
                    }

                    break;
                case "current_frame":
                    if(model.current_frame_sn === undefined || proposal.update.current_frame_sn > model.current_frame_sn) {
                        if(model.sts === undefined) {
                            model.sts = proposal.update.sts;
                        }
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
                    } else {
                        if(proposal.update.current_frame < model.current_frame) {
                            console.info("Teleview: Ignore current_frame, it is older or has same serial number as current frame.",
                                {id: model.id,
                                    keyframe_sn: model.keyframe_sn,
                                    current_frame_sn: model.current_frame_sn,
                                    update_current_frame_sn: proposal.update.current_frame_sn});
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
                        // The keyframe is sent as retained message, it will arrive almost immediately
                        model.queuedCumulativePatch = proposal.update;
                    }

                    break;
                case "incremental": // Patch against the current frame.
                    if(model.current_frame && model.current_frame_sn + 1 === proposal.update.current_frame_sn) {
                        model.current_frame = applyPatch(model.current_frame, proposal.update);
                        model.current_frame_sn = proposal.update.current_frame_sn;
                    } else {
                        model.incrementalPatchQueue.push(proposal.update);
                        setTimeout(actions.requestCurrentFrame, 0, false);
                    }

                    break;
                default:
                    throw Error("Unexpected update", proposal.type);
            }
        } else {
            // This update is older than we have right now...
            console.info("Teleview: Ignore update, it is from an older renderer.", {id: model.id, sts: model.sts, update_sts: proposal.update.sts});
        }
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
        self.publish(cotonic.mqtt.fill("model/televiewClient/+televiewId/event/stopped", model), true);
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
                setTimeout(actions.requestCurrentFrame, 0, false);
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
     * There was a current frame request error, but we need a new frame.
     */
    if(proposal.is_current_frame_request_error) {
        setTimeout(actions.requestCurrentFrame, 0, false);
    }

    if(proposal.is_renderer_down) {
        // TODO: Needs to be handled.. But not sure how.
        console.warn("Teleview: Renderer down", {id: model.id, reason: proposal.reason});
    }

    state.render(model);
}

model.requestCurrentFrame = () => {
    if(model.isCurrentFrameRequested)
        return;

    model.isCurrentFrameRequested = true;
    self.call(cotonic.mqtt.fill("bridge/origin/model/teleview/get/+teleview_id/current_frame/+renderer_id", model),
              model.pickle,
              {qos: 1})
        .then(actions.currentFrameResponse, actions.currentFrameRequestError)
        .finally(() => { model.isCurrentFrameRequested = false; });
}


/**
 * View
 */

view.display = (representation) => {
    if(!representation)
        return;

    self.publish(model.updateTopic, representation);
}

/**
 * State
 */

state.view = view;

state.render = (model) => {
    state.view.display(state.representation(model));
    state.nextAction(model) ;
}

state.isStarted = (model) => {
    return !!model.updateTopic;
}

state.hasCurrentFrame = (model) => {
    return !!model.current_frame;
}

state.isPageVisible = (model) => {
    return (model.page_state === "active" || model.page_state === "passive");
}

state.representation = (model) => {
    if(!state.isStarted(model)) return;
    if(!state.hasCurrentFrame(model)) return;
    if(!state.isPageVisible(model)) return;

    return fromUTF8(model.current_frame);
}

state.nextAction = (model) => {
    if(model.stop && model.updateTopic) {
        actions.stop();
    }
}

/**
 * Actions
 */

actions.televiewEvent = (m, a) => {
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
        default:
            console.warn("Teleview: Unknown teleview event", {id: model.id, event_type: a.evt_type});
    }
}

actions.rendererEvent = (m, a) => {
    const t = a.evt_type;

    if(t === "update") {
        actions.update(a.args[0], m.payload);
    } else if(t === "reset") {
        actions.reset();
    } else if(t === "still_watching") {
        actions.still_watching();
    } else if(t === "down") {
        actions.rendererDown(m.payload.reason);
    } else if(t === "stopped") {
        actions.rendererStopped(m.payload.reason);
    } else {
        console.warn("Teleview: Unknown renderer event", {id: model.id, event_type: t});
    }
}

actions.update = (type, update) => {
    model.present({
        is_update: true,
        type: type,
        update: update
    });
}

actions.reset = () => {
    model.present({is_reset: true});
}

actions.requestCurrentFrame = (withReset) => {
    model.present({
        is_request_current_frame: true,
        is_reset: withReset 
    });
}

actions.stop = (reason) => {
    model.present({is_stop: true, reason: reason})
}

actions.still_watching = () => {
    model.present({is_still_watching: true});
}

actions.start = (args) => {
    model.present( {is_start: true, arg: args[0] } );
}

actions.ensureServerSide = function() {
    model.present({is_ensure_server_side: true});
}

actions.handleEnsureStatus = (m, a) => {
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

actions.errorEnsureStatus = (m, a) => {
    model.present({
        is_ensure_server_status_error: true,
        arg: m.payload
    });
}

actions.lifecycleEvent = (m, a) => {
    model.present({
        is_lifecycle_event: true,
        state: m.payload
    });
}

actions.currentFrameResponse = (m) => {
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
            console.info("Teleview: Restarting", {id: model.id});
        } else {
            console.warn("Teleview: Unknown current frame response", {id: model.id});
        }
    }
}

actions.currentFrameRequestError = (m) => {
    model.present({is_current_frame_request_error: true});
}

actions.rendererDown = (reason) => {
    // The renderer is down, due to an error, or shutdown. It will be restarted
    // in case of an error.
    model.present({is_renderer_down: true, reason: reason});
}

actions.rendererStopped = function() {
    // [TODO] The renderer is permanently stopped.
}

/**
 * Helpers
 */

const toUTF8 = (() => { const e = new TextEncoder(); return e.encode.bind(e); })();
const fromUTF8 = (() => { const d = new TextDecoder(); return d.decode.bind(d); })();

function rendererEventTopic(model) {
    return `bridge/origin/model/teleview/event/${ model.teleview_id }/+evt_type/${ model.renderer_id }/#args`;
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

    // [TODO] use Array.set.
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
