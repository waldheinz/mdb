
"use strict";

/* album image show */

$(".file-thumb-list").on("click", ".file-thumb a", function(evt) {
    evt.preventDefault();
    var fileId = $(this).parent(".file-thumb").data("file-id");
    console.log(fileId);
    
    var viewer = $("#image-viewer");
    viewer.css("top", $(window).scrollTop() + "px");
    
    var image = $("<img>");
    image.attr("src", "/image/image/" + fileId);
    image.css("background-image", "url(/image/thumbnail/" + fileId + ")");
    
    viewer.children(".image-stack").append(image);
    
    viewer.show();
    viewer.focus();
    $("html").css("overflow", "hidden");
    
    history.pushState(null, null, "show/" + fileId);
});

$("#image-viewer").keyup(function(e) {
    console.log(e);
    if (e.which === 27) {
        $("html").css("overflow", "scroll");
        $(this).hide();
        $("#image-viewer .image-stack").empty();
    }
});

window.onpopstate = function(ps) {
    if (!ps.state) {
        $("html").css("overflow", "scroll");
        $("#image-viewer").hide();
        $("#image-viewer .image-stack").empty();
    }
};
