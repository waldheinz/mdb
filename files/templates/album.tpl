<apply template="default">
    <div class="row file-thumb-list">
        <div class="col-md-6">
            <h1><name/> <small>(<file-count/> files)</small></h1>
            
            <div class="row">
                <persons>
                    <div class="col-xs-4">
                        <a href="/person/${id}"
                            class="thumbnail thumblink"
                            style="background-image: url(/image/person/${id}); padding-top: 125%;"
                            />

                        <div class="thumb-bottom-overlay">
                            <div class="thumb-bottom-text">
                                <name/>
                            </div>
                        </div>
                    </div>
                </persons>
            </div>
        </div>
        
        <files>
            <div class="col-xs-2 file-thumb" data-file-id="${id}">
                <apply template="thumbnail">
                    <bind tag="target">show/${id}/</bind>
                    <bind tag="imgId">${id}</bind>
                </apply>
            </div>
        </files>
    </div>
    
    <div id="image-viewer" tabindex="1">
        <div class="image-stack">
        </div>
    </div>
</apply>
