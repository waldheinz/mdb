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
        </style>
    </head>
    <body>
    </body>
    <script type="text/javascript">
        Elm.fullscreen(Elm.Main, {
//            randomSeed: [ Math.floor(Math.random() * 0xFFFFFFFF),  Math.floor(Math.random() * 0xFFFFFFFF) ],
            initialPath: window.location.pathname
        });
    </script>
    <script src="static/js/jquery-2.2.0.min.js"></script>
    <script src="static/js/bootstrap.min.js"></script>
</html>
