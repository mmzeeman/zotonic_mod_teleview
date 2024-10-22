/**
 *
 * Teleview worker 
 *
 */

const PAGE_VISIBILITY_TIMEOUT = 5000;
const MAX_ERROR_RETRY_DELAY = 60000;

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

    hidden_start_time: undefined,
    request_error_count: 0
};

const view = {};
const state = {};
const actions = {};

/**
 * Model
 */

model.present = (proposal) => {
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
        model.current_frame_sn = arg.current_frame_sn;
        model.isCurrentFrameRequested = false;

        model.queuedCumulativePatch = undefined; 
        model.incrementalPatchQueue = [];
        model.request_error_count = 0;

        model.min_time = (arg.keyframe_min_time === undefined)?0:arg.keyframe_min_time;
        model.max_time = (arg.keyframe_max_time === undefined)?"infinite":arg.keyframe_max_time;

        model.pickle = arg.pickle;

        self.publish(cotonic.mqtt.fill("model/ui/insert/+id", model), { inner: true, priority: 10 });
        self.subscribe("model/lifecycle/event/state", actions.lifecycleEvent);
        self.publish(cotonic.mqtt.fill("model/televiewClient/+televiewId/event/started", model), true);
    }

    if(proposal.is_subscribe) {
        self.subscribe(rendererEventTopic(model), actions.rendererEvent);
    }

    if(proposal.is_reset) {
        model.sts = undefined;
        model.keyframe  = undefined;
        model.keyframe_sn = 0;

        model.current_frame = undefined;
        model.current_frame_sn = 0;
        model.isCurrentFrameRequested = false;

        model.queuedCumulativePatch = undefined; 
        model.incrementalPatchQueue = [];
        model.request_error_count = 0;
    }

    if(proposal.is_update) {
        model.handleUpdate(proposal.type, proposal.update);
    }

    if(proposal.is_current_frame_response) {
        model.request_error_count = 0;
        model.handleCurrentFrameUpdate(proposal.update);
    }

    if(proposal.is_request_current_frame) {
        if(model.request_error_count ==  0) {
            model.requestCurrentFrame();
        }
    }

    if(proposal.is_still_watching && model.updateTopic && state.isPageVisible(model)) {
        self.publish(
            cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", model),
            {});
    }

    if(proposal.is_stop && model.updateTopic) {
        self.publish(cotonic.mqtt.fill("model/televiewClient/+televiewId/event/stopped", model), true);
    }

    if(proposal.is_lifecycle_event) {
        if(model.page_state === "passive" && proposal.state === "hidden") {
            model.hidden_start_time = Date.now(); 
        }

        // Page moved from hidden to passive.
        if(model.page_state === "hidden" && proposal.state === "passive") {
            const now = Date.now();

            if(model.hidden_start_time !== undefined && ((now - model.hidden_start_time) > PAGE_VISIBILITY_TIMEOUT)) {
                setTimeout(actions.requestCurrentFrame, 0);
            }

            model.hidden_start_time = undefined;

            // Keep the teleview alive, we moved from hidden to passive.
            self.publish(
                cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", model),
                {});
        }

        model.page_state = proposal.state;
    }

    if(proposal.is_current_frame_request_error) {
        console.log(model.request_error_count);
        setTimeout(actions.requestCurrentFrame, Math.min(model.request_error_count * 1000, MAX_ERROR_RETRY_DELAY));
        model.request_error_count += 1;
    }

    if(proposal.is_renderer_down) {
        // [TODO]: Needs to be handled.. But not sure how.
        console.warn("Teleview: Renderer down", {id: model.id, reason: proposal.reason});
    }

    state.render(model);
}

model.requestCurrentFrame = () => {
    if(model.isCurrentFrameRequested)
        return;

    const args = { pickle: model.pickle };
    if(model.current_frame) {
        args.sts = model.sts;
        args.current_frame_sn = model.current_frame_sn;
    }

    model.isCurrentFrameRequested = true;
    self.call(cotonic.mqtt.fill("bridge/origin/model/teleview/get/+teleview_id/current_frame/+renderer_id", model),
        args,
        {qos: 1})
        .then(actions.currentFrameResponse, actions.currentFrameRequestError)
        .finally(() => { model.isCurrentFrameRequested = false; });
}

model.handleCurrentFrameUpdate = (update) => {
    if(model.sts === undefined
        || model.sts < update.sts
        || (model.sts === update.sts && update.current_frame_sn === model.current_frame_sn && model.current_frame === undefined)
        || (model.sts === update.sts && update.current_frame_sn > model.current_frame_sn)) {

        if(model.sts !== update.sts) {
            model.sts = update.sts;
            model.keyframe = undefined;
            model.keyframe_sn = undefined;
            model.request_error_count = 0;

            // Just in case... resubscribe to the topic. The server could have forgotton
            // the subscription because the session expired, or the server restarted.
            self.unsubscribe(rendererEventTopic(model), actions.renderEvent, actions.subscribe);
        }

        model.current_frame = toUTF8(update.current_frame);
        model.current_frame_sn = update.current_frame_sn;

        model.handleQueuedIncremental();
    } else {
        console.info("Teleview: Ignore current_frame, already up-to-date",
            {id: model.id,
                stn: model.sts,
                has_current_frame: !!model.current_frame,
                update_stn: update.sts,
                keyframe_sn: model.keyframe_sn,
                current_frame_sn: model.current_frame_sn,
                update_current_frame_sn: update.current_frame_sn});
    }
}

model.handleQueuedIncremental = () => {
    while(model.incrementalPatchQueue.length) {
        const p = model.incrementalPatchQueue.shift();

        if(model.stn !== p.stn) {
            continue; // skip
        }

        if(model.current_frame_sn + 1 === p.current_frame_sn) {
            model.current_frame = applyPatch(model.current_frame, p);
            model.current_frame_sn = p.current_frame_sn;
        } 
    }
}

model.handleUpdate = (type, update) => {
    if(type === "cumulative") {
        if(model.sts === update.sts ) {
            if(model.keyframe && model.keyframe_sn === update.keyframe_sn) {
                model.current_frame = applyPatch(model.keyframe, update);
                model.current_frame_sn = update.current_frame_sn;
            } else {
                console.warn("Cumulative patch does not fit... queue it", model.current_frame_sn, update.current_frame_sn);
                // When a keyframe arrives, it could be that it is possible to use this patch 
                // The keyframe is sent as retained message, it will arrive almost immediately
                model.queuedCumulativePatch = update;
            }
        } else {
            console.warn("Teleview: unexpected cumulative patch", {id: model.id});
        }
    } else if(type === "incremental") {
        if(model.sts === update.sts) {
            if(model.current_frame && (model.current_frame_sn + 1 === update.current_frame_sn)) {
                model.current_frame = applyPatch(model.current_frame, update);
                model.current_frame_sn = update.current_frame_sn;
            } else {
                console.warn("Patch does not fit", model.current_frame_sn, update.current_frame_sn);
                model.incrementalPatchQueue.push(update);
                setTimeout(actions.requestCurrentFrame, 0);
            }
        } else {
            console.warn("Teleview: unexpected incremental patch", {id: model.id});
        }
    } else if(type === "keyframe") {
        if(model.sts === undefined || model.keyframe_sn === undefined || update.keyframe_sn > model.keyframe_sn) {
            // Update the sts...
            if(model.sts === undefined || model.sts < update.sts) {
                model.sts = update.sts;
            }

            // Update keyframe.
            model.keyframe = toUTF8(update.frame);
            model.keyframe_sn = update.keyframe_sn;

            // Check if the new keyframe is the new current frame.
            if(model.current_frame || (model.current_frame_sn < update.keyframe_sn)) {
                model.current_frame = model.keyframe;
                model.current_frame_sn = model.keyframe_sn;
            }

            // Apply the last arrived queued cumulative patch.
            if(model.queuedCumulativePatch) {
                if(model.current_frame_sn === undefined || (model.current_frame_sn < model.queuedCumulativePatch.current_frame_sn)) {
                    if(model.queuedCumulativePatch.stn === model.stn) {
                        model.current_frame = applyPatch(model.current_frame, model.queuedCumulativePatch);
                        model.current_frame_sn = model.queuedCumulativePatch.current_frame_sn;
                    } 
                } 
                model.queuedCumulativePatch = undefined;
            }
        } else {
            console.info("Teleview: Ignore keyframe, it is older or has same serial number as current keyframe.",
                {id: model.id,
                    keyframe_sn: model.keyframe_sn,
                    update_keyframe_sn: update.keyframe_sn});
        }
    } else {
        console.warn("Teleview: unexpected update type.", {id: model.id, type: type});
    }
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

actions.requestCurrentFrame = () => {
    model.present({ is_request_current_frame: true });
}

actions.stop = (reason) => {
    model.present({is_stop: true, reason: reason})
}

actions.still_watching = () => {
    model.present({is_still_watching: true});
}

actions.start = (args) => {
    model.present( {is_start: true, arg: args[0], is_subscribe: true } );
}

actions.handleEnsureStatus = (m, _a) => {
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

actions.errorEnsureStatus = (m, _a) => {
    model.present({
        is_ensure_server_status_error: true,
        arg: m.payload
    });
}

actions.lifecycleEvent = (m, _a) => {
    model.present({
        is_lifecycle_event: true,
        state: m.payload
    });
}

actions.subscribe = () => {
    model.present({ is_subscribe: true });
}

actions.currentFrameResponse = (m) => {
    const p = m.payload;
    if(p.status === "ok") {
        if(p.result.state === "ok") {
            model.present({
                is_current_frame_response: true,
                update: p.result
            });
        } else {
            console.warn("Teleview: Unexpected current frame response", {id: model.id});
            actions.currentFrameRequestError();
        }
    }
}

actions.currentFrameRequestError = (_m) => {
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

        if(patch === "c") {
            copyInto(array, dst_idx, src, src_idx, v);
            src_idx += v;
            dst_idx += v;
        } else if(patch === "s") {
            src_idx += v;
        } else if(patch === "i") {
            const data = toUTF8(v)
            copyInto(array, dst_idx, data);
            dst_idx += data.length;
        } else {
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
