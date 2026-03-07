# zotonic_mod_teleview 📺

> Live-updating, server-rendered views for [Zotonic](https://zotonic.com) — powered by [MQTT](https://mqtt.org) and [Cotonic](https://cotonic.org).

![Teleview](https://user-images.githubusercontent.com/1024972/142467768-6838d498-2611-40dd-ae3a-60b5e474d047.jpg)

`zotonic_mod_teleview` lets you embed server-rendered HTML fragments in a Zotonic page that stay **automatically up to date** — without full-page reloads. The server pushes incremental DOM patches over MQTT; the client applies them seamlessly, just like a video stream for your UI.

## Key Features

- **Server-side rendering with live DOM updates** — no full page reloads; only the changed parts of the DOM are updated.
- **Video-like streaming updates via MQTT** — keyframe, cumulative, and incremental patches keep bandwidth minimal and clients in sync.
- **Per-renderer ACL, language, and user-specific variation** via `vary` — serve different views to admins, visitors, or individual users from a single teleview.
- **Automatic renderer lifecycle management** — renderers start on demand when a client subscribes and stop automatically when no viewers remain.
- **Built on Zotonic + Cotonic** — works seamlessly within the Zotonic ecosystem using the standard MQTT broker already present on every page.

## How It Works

### Client Side

The client side teleview component is built with [Cotonic](https://cotonic.org). This is a JavaScript library which acts as middleware.
Different components can be added. These components can then communicate by using its broker. The broker provides a [MQTT](https://mqtt.org)
publish/subscribe model. Cotonic makes it possible for a page to connect to multiple MQTT brokers. [Zotonic](https://zotonic.com) uses
the Cotonic library. Normally a page opens one MQTT connection to the server from which the page was loaded. This is called the *origin*.

![20230208144933](https://user-images.githubusercontent.com/1024972/217549349-e5ad5147-dbd9-4b9b-a7bb-e30ed303d991.png)

### Server Side

Teleview runs as a Zotonic module.

Each teleview has its own id and consists of a state process, which manages the subscription to the topics it listens to, and one or more
renderer processes which can hold private vary properties used for rendering.

![20230209171459](https://user-images.githubusercontent.com/1024972/217871548-a730156b-88f0-4eef-b207-98682fc19942.png)

*Zotonic High Level Architecture*

![20230208151717](https://user-images.githubusercontent.com/1024972/217556825-6931f2ff-84b4-46e6-a545-ffef7fd429f4.png)

*Anatomy of a Zotonic Module*

![20230209142201](https://user-images.githubusercontent.com/1024972/217824424-241f74c8-29d8-42f9-9bc1-86c06afd1546.png)

*Teleview Module*

## Getting Started

### Prerequisites

- A running [Zotonic](https://zotonic.com) installation.
- `zotonic_mod_teleview` enabled for your site.

### 1. Add a teleview to a template

Put a `{% teleview %}` scomp into your template:

```
{% teleview
    type = "example"
    template = "_example_teleview.tpl"
    topic = "model/example/event/#"
    
    extra_parameter_a = 100
    extra_parameter_b = "hello world"
    
    vary = %{ user_id = m.acl.user }
%}
```

When this scomp is placed on a page, it ensures the teleview and renderer are started. A teleview
can have multiple renderers depending on the situation — a different renderer can be started because of
authorization levels, giving different admin and visitor views, or different views per user id, language,
and other possibilities.

The scomp sends a notification `ensure_teleview`. An observer can then make sure a teleview process and
a renderer is started. When the renderer is already started, the current renderer state will be added to the
HTML output of the teleview scomp.

A piece of JavaScript code will subscribe itself to the update topic of the teleview renderer. This will keep
the view updated.

The full scomp signature is:

```
{% teleview 
       type = "<type-name>"
       template = "_a_template.tpl"
       
       topic = <an mqtt topic to listen to>
       
       teleview_param = ...
       
       vary = %{
           renderer_param: x
           ... 
       }
%}
```

### 2. Add observer hooks on the server side

Implement the following notifications in your Zotonic module to control the context used for
state management and rendering:

```erlang

observe_teleview_state_init({teleview_state_init, #{ type := <<"example">> }=Args}, Context) ->
    %% Return a context which is allowed to subscribe the topics.
    Context;
observe_teleview_state_init(_InitArgs, _Context) ->
    undefined.
    
observe_teleview_renderer_init({teleview_renderer_init, #{ type := <<"example">> }=Args}, Context) ->
    %% Return a context which allows rendering things.
    Context;
observe_teleview_renderer_init(_InitArgs, _Context) ->
    undefined.
```

Each renderer can have different parameters so they can render the interface
in different languages, for different users, or with a special set of ACL settings.

## Technical Reference

### MQTT Topics

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

A **cumulative** update is a patch against the last keyframe of a teleview. It is short for
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

*Note* Incremental updates must be applied; when they are skipped, it will not be possible to
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

### Supervision Tree

The teleview Zotonic module is a supervisor. It can start televiews. Televiews are a process which manages
the state, and a collection of renderers. The renderers take care of producing a render. Each renderer can
have its own ACL and language settings. The state process selects the right renderer for the client side of
the teleview.

![20211221152358](https://user-images.githubusercontent.com/1024972/146945749-2d6a1d65-ad8f-4f90-828d-a6f6cb8360cc.png)

