# zotonic_mod_teleview

Mod teleview provides live updating server rendered views.

📺

# Introduction

Teleview makes it possible to have server side rendered views, which update on the client. The communication
between the teleview and its client-side viewport is done via MQTT topics. Client side teleview viewports 
subscribe to a renderer, the renderer sends a video like update stream which allows the viewport to gradually
update its part of the dom-tree.

# Design

# Technical Details

## MQTT Topics

#### `model/teleview/get/topics/<id>`

Topic used to retrieve the render topics from the teleview with the given `<id>`. The selection is handled by a
`z_teleview_state` process. This process selects the right renderer based on the language and/or acl settings of
the user.

When placing a request on the request topic, the following response will be returned

```javascript
{
    "status": "ok",
    "event_topic": "model/teleview/event/<id>/<renderer-ref>/+"
}
```

#### `model/liveview/event/<id>/<renderer-ref>/+(patch_type)`

The topic on which a renderer broadcasts its update.

| patch_type | description                        |
| -----------| ---------------------------------- |
| keyframe   | A complete rendered scene.         |
| idiff      | A diff against the last keyframe.  |
| cdiff      | A diff against the last frame.     |

##### `keyframe`

A **keyframe** is the complete, text representation of a teleview. The view can be made visible
by a teleview viewport on the client side by placing it in the DOM-tree.

```javascript
{
    "ts": <timestamp>,
    "keyframe": <text>
}
```

##### `idiff`

An **idiff** is patch against the last keyframe of a teleview. It is short for *incremental patch*.
The view of this frame can be made visible by applying the patch against the text representation 
of the keyframe. It will result in a new text representation which can be placed in the DOM-tree.
The patched keyframe document is the **current frame**.

*Note* Because the patch is made against the keyframe it is possible to skip idiff patches when
the client is too busy. The view will then not be updated. The next idiff patch can be used to
update the view.

```javascript
{
    "ts": <timestamp>,
    "idiff": <patch>
}
```

##### `cdiff`

A **cdiff** is a patch against the *current frame*. It is short for *cumulative patch*. The view
of this frame can be made visible by applying the patch against the text representation of the 
current frame, and placing it in the DOM-tree. After applying the patch, the resulting text 
representation will be the new current frame.

*Note* cdiff's have to be applied, when a cdiff patch is skipped, it will not be possible to
construct a new current frame by applying patches. The teleview viewport will have to wait for 
a new keyframe. Because of this drawback, teleview renderer will try to send idiffs as often as
possible. Only when the patch against the keyframe becomes too complex, and the minimum time
between keyframes is passed, a cdiff will be sent.

```javascript
{
    "ts": <timestamp>,
    "cdiff": <patch>
}
```

##### Diff

The diff is a list of instructions.

   - "c" for copy
   - "s" for skip
   - "i" for insert

Example:

```json
["c", 100, "s", 51, "i", "<b>Hello World</b>", "c", 57]
```

Which means, copy 100 characters from the current document to the new, skip 51 characters from the current document, insert "<b>Hello World</b>", and copy 57 characters from the current document to the new one.

## Supervision Tree

The teleview zotonic module is a supervisor. It can start televiews. Televiews are a process which manages
the state, and a collection of renderers. The renderers take care of producing a render. Each renderer can 
have its own ACL and language settings. The state process selects the right renderer for the client side of
the teleview.

![20191214105659](https://user-images.githubusercontent.com/1024972/70848092-c9d98b00-1e6c-11ea-90fb-13b88b98ad0a.png)


