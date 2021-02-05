

function newTeleviewState() {
    return {
        keydoc: undefined,
        keydoc_ts: undefined,

        current: undefined,
        current_ts: undefined
    };
}

function updateDoc(type, update, state) {
    // Note: a timestamp should be used to check if the update can
    // be applied to the current state. This is skipped right now.

    console.log(type, update, state);
    
    switch(type) {
        case "keyframe":
            state.keydoc = state.current = update.frame;
            state.keydoc_ts = state.current_ts = update.ts;
            break;
        case "incremental": // Patch against the current doc.
            if(!state.current) {
                console.log("waiting for current");
            } else {
                state.current = applyPatch(state.current, update.patch);
                state.current_ts = update.ts
            }

            break;
        case "cumulative": // Patch against the keydoc
            if(!state.keydoc) {
                console.log("waiting for keydoc");
            } else {
                state.current = applyPatch(state.keydoc, update.patch);
                state.current_ts = update.ts;
            }

            break;
        default:
            throw Error("unexpected update", type);
    }

    return state;
}

function applyPatch(source, patches) {
    if(patches.length === 0)
        return source;

    const src = source; //encoder.encode(source);
    const dst = [];

    let idx = 0;
    for(let i=0, l=patches.length; i < l; i += 2) {
        let patch = patches[i];
        let v = patches[i+1];

        switch(patch) {
            case "c": // copy
                const slice = src.slice(idx, idx+v);

                // dst.push(decoder.decode(slice));
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
