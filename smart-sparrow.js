// create a data model that exposes parameters to smart sparrow
var model = new pipit.CapiAdapter.CapiModel({
    mastery: false,
    numerator: 30,
    denominator: 50,
    implementMastery : false,
    debug : false
});

//Tells pipit to expose the following attributes
pipit.CapiAdapter.expose('mastery', model);
pipit.CapiAdapter.expose('numerator', model);
pipit.CapiAdapter.expose('denominator', model);
pipit.CapiAdapter.expose('implementMastery', model);
pipit.CapiAdapter.expose('debug', model);

// Tells pipit that the sim model is ready
pipit.Controller.notifyOnReady();

var node = document.getElementById('my-thing');
var app = Elm.ListOperations.embed(node);

app.ports.updateMastery.subscribe(function(mastery) {
    model.set('mastery', mastery);
    alert("Congratulations! You're ready to move on. Press the NEXT button to advance to the next slide.");
    pipit.Controller.triggerCheck();
});

model.on('change:numerator', function(){
    // alert("numerator changed");
    app.ports.ssData.send( model );
});

model.on('change:denominator', function(){
    // alert("denominator changed");
    app.ports.ssData.send( model );
});

model.on('change:implementMastery', function(){
    // alert("impMastery changed");
    app.ports.ssData.send( model );
});

model.on('change:debug', function(){
    // alert("debug changed");
    app.ports.ssData.send( model );
});

$(document).ready(function() {
    // alert("here i am");
});
