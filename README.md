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

#### `model/teleview/get/<id>/state/<renderer-ref>`

Returns the current state of a renderer of a teleview.

#### `model/teleview/post/<id>/still-watching/<renderer-ref>`

#### `model/teleview/event/<id>/started`

A teleview with id `id` was started.

#### `model/teleview/event/<id>/stopped`

A teleview with id `id` was stopped.

#### `model/teleview/event/<id>/reset/<renderer-ref>`

The teleview was reset, possibly because of a crash. It was restarted, meaning that
the current state of the teleview viewer has to be reset.

#### `model/teleview/event/<id>/still-watching/<renderer-ref>`

A renderer will send `still-watching` messages to its viewers. When it does not
receive a response, the renderer assumes that nobody is interested in updates, 
and will stop itself. When a teleview has no more renderers, it will be stopped 
too.

#### `model/teleview/event/<id>/update/<renderer-ref>/+(patch_type)`

The topic on which a renderer broadcasts its update.

| patch_type  | description                        |
| ----------- | ---------------------------------- |
| keyframe    | A complete rendered tree element.  |
| cumulative  | A diff against the last keyframe.  |
| incremental | A diff against the last frame.     |

##### `keyframe`

A **keyframe** is the complete, text representation of a teleview. The view can be made visible
by a teleview viewport on the client side by placing it in the DOM-tree.

```javascript
{
    "sn": <serial-number>,
    "frame": <text>
}
```

##### `cumulative`

An **cumulative** update is a patch against the last keyframe of a teleview. It is short for
*cumulative patch*. The view of this frame can be made visible by applying the patch against the
text representation of the keyframe with the specified serial number. It will result in a new
text representation which can be placed in the DOM-tree. The patched keyframe document is
the **current frame**.

*Note* Because the patch is made against the keyframe it is possible to skip cumulative updates
when the client is too busy. The view will then not be updated. The next cumulative patch can be
used to update the view.

```javascript
{
    "sn": <serial>,
    "patch": <patch>
}
```

##### `incremental`

An **incremental** update is a patch against the *current frame*. The view of this frame can
be made visible by applying the patch against the text representation of the 
current frame, and placing it in the DOM-tree. After applying the patch, the resulting text 
representation will be the new current frame.

*Note* incremental updates must be applied, when they are skipped, it will not be possible to
construct a new current frame by applying patches. The teleview viewer will have to wait for 
a new keyframe. Because of this drawback, teleview renderer will try to send cumulative updates
as often as possible. Only when the patch against the keyframe becomes too complex, or the minimum time
between keyframes is passed, an incremental patch will be sent.

```javascript
{
    "sn": <timestamp>,
    "patch": <patch>
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


