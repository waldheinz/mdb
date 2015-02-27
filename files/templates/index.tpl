<apply template="default">
    <h2>Persons</h2>
    <div class="row">
        <persons>
            <div class="col-xs-2">
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
</apply>
