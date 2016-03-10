
var make = function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.VideoPlayer = elm.Native.VideoPlayer || {};

    if (elm.Native.VideoPlayer.values) return elm.Native.VideoPlayer.values;

    return elm.Native.VideoPlayer.values = {};
};

Elm.Native.VideoPlayer = {};
Elm.Native.VideoPlayer.make = make;
