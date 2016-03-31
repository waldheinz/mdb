
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

        try {
            if (play) {
                if (model.doSeek) {
                    elem.src = model.videoBaseUrl + "?t=" + model.playStartTime;
                }

                elem.play();
            } else {
                elem.pause();
            }
        } catch (ex) {
            console.log("playing video failed: " + ex);
        } finally {
            return Task.succeed({ ctor : "SeekDone" });
        }
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

    var cookies;

    function readCookie(name){
        if(cookies){ return cookies[name]; }

        var c = document.cookie.split('; ');
        cookies = {};

        for(var i=c.length-1; i>=0; i--){
           var C = c[i].split('=');
           cookies[C[0]] = C[1];
        }

        return cookies[name];
    }

    function doAttachHls(model) {
        return Task.asyncFunction(function(callback) {
            var elem = document.getElementById(model.playerId);
            var hls = new Hls({
                debug : true
            });
            
            hls.loadSource(model.videoBaseUrl + "/variants");
            hls.attachMedia(elem);
            return callback(Task.succeed(noOp));
        });
    }

    return elm.Native.VideoPlayer.values = {
        setPlay         : F2(doSetPlay),
        goFullscreen    : doGoFullscreen,
        attachHls       : doAttachHls
    };
};
