<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <base href="/" />
        <title>MDB</title>
        <link href="static/css/bootstrap.min.css" rel="stylesheet">

        <script src="static/js/mdb.js"></script>
        <style>
            .thumb-container {
                display: flex;
                flex-wrap: wrap;
                justify-content: space-between;
                margin: -1px -1px;
            }

            .thumb-container:after {
                content: "";
                flex: auto;
            }

            .file-thumb {
                margin: 1px 1px;
            }

            .file-thumb  img {
                height: 128px;
            }

            .video-responsive {
                background-color: black;
            }

            .video-controls {
                position: absolute;
                bottom: 0;
                width: 100%;
                padding-top: 10px;
                background: linear-gradient(to bottom, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75));
                transition:opacity 0.25s ease;
            }

            .video-progress {
                width: 100%;
                height: 8px;
                background-color: rgba(255, 255, 255, 0.5);
                cursor: pointer;
            }

            .video-playtime {
                color: white;
                padding: 5px;
            }

            .video-button {
                background-color: transparent;
                border: none;
                color: white;
                padding: 5px;
                font-size: 20px;
                outline: none;
            }
        </style>
    </head>
    <body>
    </body>
    <script type="text/javascript">
        Elm.fullscreen(Elm.Main, {
            initialPath: window.location.pathname
        });
    </script>
    <script src="static/js/jquery.min.js"></script>
    <script src="static/js/bootstrap.min.js"></script>
</html>
