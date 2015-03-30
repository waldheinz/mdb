<apply template="default">
    
    <h2>Albums</h2>
    
    <nav>
        <div class="text-center">
            <ul class="pagination">
                <pagination>
                    <pages>
                        <li class="${class}"><a href="${page}"><page/></a>
                    </pages>
                </pagination>
            </ul>
        </div>
    </nav>
    
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
    
</apply>
