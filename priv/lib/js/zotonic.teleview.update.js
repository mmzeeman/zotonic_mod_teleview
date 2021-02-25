/**
 *
 * First version of the teleview viewer code.
 *
 * Needs to be packaged more robustly.
 */

function initTeleviewer(state) {
    console.log(state);

    const uiInsertTopic = cotonic.mqtt.fill("model/ui/insert/+uiId", {uiId: state.uiId});
    const uiUpdateTopic = cotonic.mqtt.fill("model/ui/update/+uiId", {uiId: state.uiId});

    const encoder = new TextEncoder();
    const decoder = new TextDecoder();

    const tvState = {
        keyframe: encoder.encode(state.keyframe),
        keyframe_sn: state.keyframe_sn,

        current_frame: (state.current_frame===undefined) ? undefined : encoder.encode(state.current_frame),
        current_frame_sn: state.current_frame_sn,

        max_time: state.max_time,
        min_time: state.min_time,

        current_text: function() {
            let t = decoder.decode(tvState.current_frame);
            return t;
        },

        encoder: encoder,
        decoder: decoder,
    };

    /*
     */
    cotonic.broker.publish(
        uiInsertTopic,
        {
            initialData: undefined,
            inner: true,
            priority: 10
        }
    );

    const evtTopic = "bridge/origin/model/teleview/event/"
        + state.teleview_id
        + "/+evt_type/"
        + state.renderer_id
        + "/#args";

    cotonic.broker.subscribe(evtTopic, 
        function(m, a) {
            switch(a.evt_type) {
                case "update":
                    updateState(a.args[0], m.payload, tvState);

                    if(tvState.current_frame !== undefined) {
                        cotonic.broker.publish(uiUpdateTopic, tvState.current_text());
                    }

                    break;
                case "reset":
                    tvState.keyframe  = undefined;
                    tvState.keyframe_sn = 0;
                    tvState.current_frame = undefined;
                    tvState.current_frame_sn = 0;

                    break;
                case "still_watching":
                    cotonic.broker.publish(
                        cotonic.mqtt.fill("bridge/origin/model/teleview/post/+teleview_id/still_watching/+renderer_id", state),
                        {});

                    break;
            }
        }
    )
}

/*
 *
 */
function updateState(type, update, state) {
    switch(type) {
        case "keyframe":
            if(state.keyframe_sn === undefined || update.keyframe_sn > state.keyframe_sn) {
                state.keyframe = state.current_frame = state.encoder.encode(update.frame);
                state.keyframe_sn = state.current_frame_sn = update.keyframe_sn;
            } else {
                console.log("Keyframe in update is older than current state.");
            }
            break;
        case "incremental": // Patch against the current doc.
            if(!state.current_frame) {
                console.log("waiting for current frame");
            } else {
                if(state.current_frame_sn === update.current_frame_sn) {
                    state.current_frame = applyPatch(state.current_frame, update, state.encoder);
                    state.current_frame_sn = update.current_frame_sn;
                } else {
                    console.log("Incremental patch does not match current frame sn");
                }
            }

            break;
        case "cumulative": // Patch against the keyframe
            if(!state.keyframe) {
                console.log("waiting for keyframe");
            } else {
                if(state.keyframe_sn === update.keyframe_sn) {
                    state.current_frame = applyPatch(state.keyframe, update, state.encoder);
                    
                    // Update the current frame sn when it is available. 
                    if(update.current_frame_sn !== undefined) {
                        state.current_frame_sn = update.current_frame_sn;
                    }
                } else {
                    console.log("Cumulative patch does not match keyframe sn");
                }
            }

            break;
        default:
            throw Error("Unexpected update", type);
    }

    return state;
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
