// pull in desired CSS/SASS files
require( './styles/main.scss' );
var KBAudio = require( './js/kbaudio.js' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

var kba = new KBAudio;



app.ports.audioStart.subscribe(function(nevermind) {
    kba.start();
});

app.ports.audioStop.subscribe(function(nevermind) {
    kba.stop();
});

app.ports.notesChanged.subscribe(function(nl) {
    // Logger.debug(nl);
    kba.notesHaveChanged(nl);
})

app.ports.bpmChanged.subscribe(function(newBPM) {
    kba.changeBPM(newBPM);
});
