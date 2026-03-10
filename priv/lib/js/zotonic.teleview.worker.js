/**
 * Copyright 2019-2026 Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS-IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

"use strict";

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
    pickle: undefined,

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

        model.pickle = arg.pickle;

        self.publish(cotonic.mqtt.fill("model/ui/insert/+id", model), { inner: true, priority: 10 });

        self.subscribe("model/lifecycle/event/state", actions.lifecycleEvent);
        self.publish(cotonic.mqtt.fill("model/televiewClient/+televiewId/event/started", model), true);
    }

    if(proposal.is_subscribe) {
        self.subscribe(televiewEventTopic(model), actions.televiewEvent);
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

    if(proposal.is_keyframe) {
        model.handleKeyFrame(proposal.update);
    }

    if(proposal.is_cumulative) {
        model.handleCumulative(proposal.update);
    }

    if(proposal.is_incremental) {
        model.handleIncremental(proposal.update);
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

    if(proposal.is_ping && model.updateTopic && state.isPageVisible(model)) {
        self.publish(
            cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/ping/+renderer_id", model),
            {});
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
                cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/ping/+renderer_id", model),
                {});
        }

        model.page_state = proposal.state;
    }

    if(proposal.is_current_frame_request_error) {
        setTimeout(actions.requestCurrentFrame, Math.min(model.request_error_count * 1000, MAX_ERROR_RETRY_DELAY));
        model.request_error_count += 1;
    }

    if((proposal.is_renderer_down || proposal.is_teleview_down) && ((model.page_state === "passive") || (model.page_state === "active"))) {
        // The view is visible, try to restart the it by requesting the current frame
        setTimeout(actions.requestCurrentFrame, 0);
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
            self.unsubscribe([ televiewEventTopic(model), rendererEventTopic(model) ], undefined, actions.subscribe);
        }

        model.current_frame = toUTF8(update.current_frame);
        model.current_frame_sn = update.current_frame_sn;

        model.handleQueuedIncremental();
    }
}

model.handleQueuedIncremental = () => {
    while(model.incrementalPatchQueue.length) {
        const p = model.incrementalPatchQueue.shift();

        if(model.sts !== p.sts) {
            continue; // skip
        }

        if(model.current_frame_sn + 1 === p.current_frame_sn) {
            model.current_frame = applyPatch(model.current_frame, p);
            model.current_frame_sn = p.current_frame_sn;
        } 
    }
}

model.handleKeyFrame = (update) => {
    if(model.sts === undefined || model.keyframe_sn === undefined || update.keyframe_sn > model.keyframe_sn) {
        // Update the sts...
        if(model.sts === undefined || model.sts < update.sts) {
            model.sts = update.sts;
        }

        // Update keyframe.
        model.keyframe = toUTF8(update.frame);
        model.keyframe_sn = update.keyframe_sn;

        // Check if the new keyframe is the new current frame.
        if(!model.current_frame || (model.current_frame_sn < update.keyframe_sn)) {
            model.current_frame = model.keyframe;
            model.current_frame_sn = model.keyframe_sn;
        }

        // Apply the last arrived queued cumulative patch.
        if(model.queuedCumulativePatch) {
            if(model.current_frame_sn === undefined || (model.current_frame_sn < model.queuedCumulativePatch.current_frame_sn)) {
                if(model.queuedCumulativePatch.sts === model.sts) {
                    model.current_frame = applyPatch(model.current_frame, model.queuedCumulativePatch);
                    model.current_frame_sn = model.queuedCumulativePatch.current_frame_sn;
                } 
            } 
            model.queuedCumulativePatch = undefined;
        }
    } 
}

model.handleCumulative = (update) => {
    if(model.sts === update.sts ) {
        if(model.keyframe && model.keyframe_sn === update.keyframe_sn) {
            model.current_frame = applyPatch(model.keyframe, update);
            model.current_frame_sn = update.current_frame_sn;
        } else {
            // When a keyframe arrives, it could be that it is possible to use this patch 
            // The keyframe is sent as retained message, it will arrive almost immediately
            model.queuedCumulativePatch = update;
        }
    } 
}

model.handleIncremental = (update) => {
    if(model.sts === update.sts) {
        if(model.current_frame && (model.current_frame_sn + 1 === update.current_frame_sn)) {
            model.current_frame = applyPatch(model.current_frame, update);
            model.current_frame_sn = update.current_frame_sn;
        } else {
            // Queue the patch...
            model.incrementalPatchQueue.push(update);
            setTimeout(actions.requestCurrentFrame, 0);
        }
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
    // no next action needed.
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

/**
 * Actions
 */

actions.start = (args) => {
    model.present( {is_start: true, arg: args[0], is_subscribe: true } );
}

actions.televiewEvent = (m, a) => {
    switch(a.evt_type) {
        case "start":
            console.warn("Teleview: start", m, model);
            break;
        case "down":
            model.present({is_teleview_down: true, reason: m.payload.reason});
            break;
        default:
            console.warn("Teleview: Unknown teleview event", {id: model.id, event_type: t});
    }
}

actions.rendererEvent = (m, a) => {
    switch(a.evt_type) {
        case "start":
            model.present({ is_reset: true });
            break;
        case "ke":
            model.present({ is_keyframe: true, update: m.payload });
            break;
        case "cu":
            model.present({ is_cumulative: true, update: m.payload });
            break;
        case "in":
            model.present({ is_incremental: true, update: m.payload });
            break;
        case "ping":
            model.present({ is_ping: true });
            break;
        case "down":
            model.present({ is_renderer_down: true, reason: m.payload.reason });
            break;
        default:
            console.warn("Teleview: Unknown renderer event", {id: model.id, event_type: a.evt_type});
    }
}

actions.requestCurrentFrame = () => {
    model.present({ is_request_current_frame: true });
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

/**
 * Helpers
 */

const toUTF8 = (() => { const e = new TextEncoder(); return e.encode.bind(e); })();
const fromUTF8 = (() => { const d = new TextDecoder(); return d.decode.bind(d); })();

function televiewEventTopic(model) {
    return `bridge/origin/model/teleview/event/${ model.teleview_id }/+evt_type`;
}

function rendererEventTopic(model) {
    return `bridge/origin/model/teleview/event/${ model.teleview_id }/+evt_type/${ model.renderer_id }`;
}

function applyPatch(source, update) {
    if(update.patch.length === 0)
        return source;

    const buffer = new ArrayBuffer(update.result_size);
    const array = new Uint8Array(buffer);
    const src = source;
    const COPY = 67, SKIP = 83, INSERT = 73; 

    let src_idx = 0;
    let dst_idx = 0;

    for(let i = 0, l = update.patch.length; i < l; i += 2) {
        const patch = update.patch[i];
        const v = update.patch[i+1];

        if(patch == COPY) {
            array.set(src.subarray(src_idx, src_idx+v), dst_idx);
            src_idx += v;
            dst_idx += v;
        } else if(patch == SKIP) {
            src_idx += v;
        } else if(patch == INSERT) {
            const data = toUTF8(v);
            array.set(data, dst_idx);
            dst_idx += data.length;
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
