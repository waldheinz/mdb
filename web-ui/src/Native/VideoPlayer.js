
Elm.Native.VideoPlayer = {};
Elm.Native.VideoPlayer.make = function(elm) {
    "use strict";

    elm.Native = elm.Native || {};
    elm.Native.VideoPlayer = elm.Native.VideoPlayer || {};

    if (elm.Native.VideoPlayer.values) return elm.Native.VideoPlayer.values;

    var Task = Elm.Native.Task.make(elm);
    var noOp = { ctor : "NoOp" };

    function doSetPlay(model, play) {
        var elem = document.getElementById(model.playerId);

        if (play) {
            if (model.doSeek) {
                elem.src = model.videoBaseUrl + "?t=" + model.playStartTime;
            }

            elem.play();
        } else {
            elem.pause();
        }

        Task.succeed({ ctor : "SeekDone" });
    }

    function doGoFullscreen(model) {
        var elem = document.getElementById(model.playerId + "-container");

        if (elem) {
            try {
                if (elem.requestFullscreen) {
                    elem.requestFullscreen();
                } else if (elem.msRequestFullscreen) {
                    elem.msRequestFullscreen();
                } else if (elem.mozRequestFullScreen) {
                    elem.mozRequestFullScreen();
                } else if (elem.webkitRequestFullscreen) {
                    elem.webkitRequestFullscreen();
                }
            } catch (ex) {
                /* what shall one do? */
            }
        }

        return Task.succeed(noOp);
    }

    return elm.Native.VideoPlayer.values = {
        setPlay         : F2(doSetPlay),
        goFullscreen    : doGoFullscreen
    };
};
