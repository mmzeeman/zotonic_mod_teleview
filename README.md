# zotonic_mod_teleview

Mod teleview provides live updating server rendered views.

📺

# Introduction

Teleview makes it possible to have server side rendered views, which update on the client. The communication
between the teleview and its client-side viewport is done via MQTT topics. Client side teleview viewports 
subscribe to a renderer, the renderer sends a video like update stream which allows the viewport to gradually
update its part of the dom-tree.

# Design

[TODO]

## Supervision Tree

The teleview zotonic module is a supervisor. It can start televiews. Televiews are a process which manages
the state, and a collection of renderers. The renderers take care of producing a render. Each renderer can 
have its own ACL and language settings. The state process selects the right renderer for the client side of
the teleview.

![20191214105659](https://user-images.githubusercontent.com/1024972/70848092-c9d98b00-1e6c-11ea-90fb-13b88b98ad0a.png)


