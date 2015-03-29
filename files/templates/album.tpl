<apply template="default">
    
    
    <div class="row">
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
            <div class="col-xs-2" >
                <apply template="thumbnail">
                    <bind tag="target">/show/${id}</bind>
                    <bind tag="imgId">${id}</bind>
                </apply>
            </div>
        </files>
    </div>
</apply>
