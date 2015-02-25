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
                <a href="/album/${id}" class="thumbnail">
                    <img src="/image/thumbnail/${poster}">
                </a>
            </div>
        </albums>
    </div>

    <h2>Random Files</h2>

    <div class="row">
        <files>
            <div class="col-xs-2">
                <a href="/show/${id}" class="thumbnail">
                    <img src="/image/thumbnail/${id}">
                </a>
            </div>
        </files>
    </div>
</apply>
