
Elm.Native.VideoPlayer = {};
Elm.Native.VideoPlayer.make = function(elm) {
    "use strict";

    elm.Native = elm.Native || {};
    elm.Native.VideoPlayer = elm.Native.VideoPlayer || {};

    if (elm.Native.VideoPlayer.values) return elm.Native.VideoPlayer.values;

    var Task = Elm.Native.Task.make(elm);
    var noOp = { ctor : "NoOp" };

    function doSetPlay(model, play) {
        return Task.asyncFunction(function (callback) {
            var elem = document.getElementById(model.playerId);

            if (play) {
                elem.src = model.videoBaseUrl + "?t=" + model.playStartTime;
                elem.play();
            } else {
                elem.pause();
            }

            return callback(Task.succeed(noOp));
        });
    }

    return elm.Native.VideoPlayer.values = {
        setPlay : F2(doSetPlay)
    };
};
