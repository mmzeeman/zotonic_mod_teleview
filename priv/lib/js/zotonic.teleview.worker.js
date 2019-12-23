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

model.eventTopic = null;
model.keyframe = null;
model.current_frame = null;

model.present = function(data) {

    if(!state.isStarted(model)) {
        self.connect();
        model.started = true;
    }

    if(!state.isConnected(model) && data.connected) {
        model.connected = true;
    }

    if(state.isConnected(model) && data.getEventTopic) {
        console.log("call teleview model");
        self.call("bridge/origin/model/teleview/get/topics/my_name")
            .then(actions.topics, actions.topicsError);

        model.eventTopic = true; // is being requested 
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
    console.log("nextAction", model);

    if(state.isConnected(model) && !model.eventTopic) {
        console.log("request event topic");
        actions.getEventTopic();
    }
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
    // [TODO] We can only start when the page is connected.
    setTimeout(function() {
        model.present({});
    }, 2000);
}

actions.on_connect = function() {
    model.present({connected: true});
}

actions.getEventTopic = function() {
    model.present({getEventTopic: true});
}

actions.topics = function(d) {
    console.log(d);
}

actions.topicsError = function(d) {
    console.log(d);
}

/**
 * Worker Startup
 */

self.on_connect = actions.on_connect;

actions.start();
