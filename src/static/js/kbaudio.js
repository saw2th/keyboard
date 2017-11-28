var Tone = require( "Tone" );

function KBAudio() {
    this.tempo = 120;
    this.current_note = 0;
    this.synth = new Tone.FMSynth(
	{
	    "harmonicity":5,
	    "modulationIndex": 1,
	    "oscillator" : {
		"type": "triangle"
	    },
	    "envelope": {
		"attack": 0.001,
		"decay": 2,
		"sustain": 0.1,
		"release": 3
	    },
	    "modulation" : {
		"type" : "square"
	    },
	    "modulationEnvelope" : {
		"attack": 0.002,
		"decay": 0.2,
		"sustain": 0,
		"release": 0.2
	    }
	}   ).toMaster();
    Tone.Transport.bpm.value = this.tempo;

    var that = this;
    this.pattern = new Tone.Sequence(function(time, pitch) {
        that.synth.triggerAttackRelease(pitch, "2n", time);
    }, [], "8n");


}

KBAudio.prototype = {
    start: function() {
        this.pattern.loop = 99999;
	this.pattern.start();
        Tone.Transport.start('+0.1');
    },

    stop: function() {
        Tone.Transport.stop();
        Tone.Transport.cancel(0);
        Tone.Transport.position = "00:00:00";
    },

    notesHaveChanged: function(newNotes) {
        this.runLength = newNotes.length;
        this.pattern.loopEnd = '8n * ' + (this.runLength);
        for (var i = 0; i < this.runLength; i++) {
	    this.pattern.at(i, newNotes[i]);
        }
    },

    changeBPM: function(newBPM) {
	Tone.Transport.bpm.value = newBPM;
    }
};

module.exports = KBAudio;
