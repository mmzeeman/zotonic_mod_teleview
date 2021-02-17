

function newTeleviewState(state) {
    return {
        keyframe: state.keyframe,
        keyframe_sn: state.keyframe_sn,

        current_frame: state.current_frame,
        current_frame_sn: state.current_frame_sn,

        max_time: state.max_time,
        min_time: state.min_time,

        publish_topic: state.publish_topic
    };
}

function updateDoc(type, update, state) {
    // Note: a timestamp should be used to check if the update can
    // be applied to the current state. This is skipped right now.
    //

    console.log(type);
    
    switch(type) {
        case "keyframe":
            if(update.keyframe_sn > state.keyframe_sn) {
                state.keyframe = state.current_frame = update.frame;
                state.keyframe_sn = state.current_frame_sn = update.keyframe_sn;

                console.log("New keyframe", state.keyframe_sn);
            } else {
                console.log("Keyframe is not an update");
            }
            break;
        case "incremental": // Patch against the current doc.
            if(!state.current_frame) {
                console.log("waiting for current frame");
            } else {
                if(state.current_frame_sn === update.current_frame_sn) {
                    state.current_frame = applyPatch(state.current_frame, update.patch);
                    state.current_frame_sn = update.current_frame_sn;

                    console.log("New current_frame", state.keyframe_sn);
                } else {
                    console.log("Incremental patch does not match sn");
                }
            }

            break;
        case "cumulative": // Patch against the keyframe
            if(!state.keyframe) {
                console.log("waiting for keyframe");
            } else {
                if(state.keyframe_sn === update.keyframe_sn) {
                    state.current_frame = applyPatch(state.keyframe, update.patch);
                    
                    // Update the current frame sn when it is available. 
                    state.current_frame_sn = update.current_frame_sn;

                    console.log("New current_frame", update.patch, state.current_frame, state.keyframe_sn);
                } else {
                    console.log("Cumulative patch does not match sn");
                }
            }

            break;
        default:
            throw Error("Unexpected update", type);
    }

    return state;
}

function applyPatch(source, patches) {
    if(patches.length === 0)
        return source;

    const src = source;
    const dst = [];

    let idx = 0;
    for(let i=0, l=patches.length; i < l; i += 2) {
        let patch = patches[i];
        let v = patches[i+1];

        switch(patch) {
            case "c": // copy
                const slice = src.slice(idx, idx+v);
                dst.push(slice);

                idx += v;
                break;
            case "s": // skip
                idx += v;
                break;
            case "i": // insert
                dst.push(v);
                break;
            default:
                throw Error("Unexpected patch");
        }
    }

    return dst.join("");
}
