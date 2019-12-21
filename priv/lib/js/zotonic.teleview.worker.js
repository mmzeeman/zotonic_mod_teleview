/**
 * [TODO] add copyright notice
 */

"use strict";

/**
 * Define SAM objects
 */

let model = {};
let view = {};
let state = {};
let actions = {};

/**
 * Model
 */

model.started = false;
model.connected = false;
model.keyframe = null;
model.current_frame = null;

model.present = function(data) {

    if(!state.isStarted(model)) {
        self.connect();
        self.started = true;
    }

    if(!state.isConnected(model) && data.connected) {
        self.connected = true;
    }


    state.render(model);
}

/**
 * View
 */

view.display = function(representation) {
}

/**
 * State
 */

state.view = view;
model.state = state;

state.isStarted = function(model) {
    return model.started === true ;
}

state.isConnected = function(model) {
    return model.started === true && model.connected === true;
}

state.nextAction = function(model) {
}

state.representation = function(model) {
}

state.render = function(model) {
    state.representation(model);
    state.nextAction(model);
}

/**
 * Actions
 */

actions.start = function() {
    model.present({});
}

actions.connected = function() {
    model.present({connected: true});
}

/**
 * Worker Startup
 */

self.on_connect = actions.connected;

actions.start();
