
function comp(name, props) {
  props.template = '#' + name;
  Vue.component(name, props);
}

function getChoice() { getChoice_(app, this.choice.choiceId); }

function getChoice_(o,i) {
  getChoicesByChoiceId(i,
    function(x){ o.choice = x; },
    function(x){ app.errors.push("Couldn't fetch choice " + i); }
  ); }

function newChoice() {
  postChoices( {choiceName: this.choiceName},
    function(choiceResponse) { getChoices_() },
    function(x) { console.log(x); app.errors.push("Could not create choice due to error "); }
  ) }

function newOption() {
  var choiceId = this.choice.choiceId;
  var optName  = this.optionName;
  if(! optName || optName == "") {
    app.errors.push("Please fill in the option name ");
    return;
  }
  postChoicesByChoiceIdAdd(
    choiceId,
    { optionName: optName, optionChoiceId: choiceId},
    function(optionResponse) { getChoice.call({choice: {choiceId: choiceId}}); },
    function(err)            { console.log(err); app.errors.push("Error adding option to choice "); }
  )}

function decide() {
  var choiceId = this.option.optionChoiceId;
  var option   = this.option;
  postChoicesByChoiceIdChoose(
    choiceId,
    option.optionId,
    function() {
      getChoice.call({choice: {choiceId: choiceId}})
    })}

function removeAllErrors() {
  app.errors = [];
}

function getChoices_() {
  getChoices(function(cs){
    app.choices = cs;
  }) }

comp('choices-list', { props:    ['choices', 'choiceName'],
                       methods:  { newChoice: newChoice },
                       computed: { revChoices: function() { return this.choices.reverse() } }});

comp('choice-item',  { props:    ['choice'],
                       methods:  { clickChoice: getChoice } });

comp('choice-info',  { props:    ['choice', 'options', 'decision', 'optionName'],
                       methods:  { newOption: newOption },
                       computed: { revOptions: function() { return this.options.reverse() } } });

comp('option-item',  { props:    ['option'],
                       methods:  { clickOption: decide } });

comp('errors',       { props:    ['errors'],
                       methods:  { removeAllErrors: removeAllErrors } });

comp('router',       { props:    ['choice'] });

var routerComponent = {
  name:     "RouterTemplate",
  props:    [ 'choice' ],
  created:  function() { this.fetchData() },
  watch:    { '$route': 'fetchData' },
  methods:  { fetchData: function() { getChoice_(this.$router.app, this.$route.params.id); } },
  template: '<router :choice="choice"></router>'
}

var router_template = '<p> <em> Click on or create a choice... </em> </p>';

var router = new VueRouter({
  routes: [ { path: '/choices/:id', component: routerComponent },
            { path: '/',            component: { props:    ['choice'],
                                                 template: router_template }}]});

var app = new Vue({
  el:     '#app',
  router: router,
  data:   { choices: [], choice: null, errors: [] }
});

getChoices_();
