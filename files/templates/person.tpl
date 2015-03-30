<apply template="default">
    <person>
        <h2><name/></h2>
        <div class="row">
            <div class="col-xs-4">
                <img class="img-thumbnail" src="/image/person/${id}">
            </div>
        </div>
    </person>

    <h2>Albums</h2>

    <div class="row">
        <albums>
            <div class="col-xs-2">
                <apply template="thumbnail">
                    <bind tag="target">/album/${id}/</bind>
                    <bind tag="imgId">${poster}</bind>
                </apply>
                <name/>
            </div>
        </albums>
    </div>

    <h2>Random Files</h2>

    <div class="row file-thumb-list">
        <files>
            <div class="col-xs-2 file-thumb">
                <apply template="thumbnail">
                    <bind tag="target">/show/${id}</bind>
                    <bind tag="imgId">${id}</bind>
                </apply>
            </div>
        </files>
    </div>
</apply>
