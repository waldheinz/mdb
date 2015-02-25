<apply template="default">
    <h2>Random Files</h2>

    <div class="row">
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
