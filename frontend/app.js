
function comp(name, props) {
  props.template = '#' + name;
  Vue.component(name, props);
}

function getChoice() {
  getChoicesByChoiceId(this.choice.choiceId, function(x){
    app.choice = x;
  })
}

function getChoice_(o,i) {
  getChoicesByChoiceId(i, function(x){ o.choice = x; });
}

function newChoice() {
  postChoices( {choiceName: this.choiceName}, function(choiceResponse) {
    getChoices_()
  })
}

function newOption() {
  var choiceId = this.choice.choiceId;
  postChoicesByChoiceIdAdd(
    choiceId,
    { optionName: this.optionName,
      optionChoiceId: choiceId},
    function(optionResponse) {
      getChoice.call({choice: {choiceId: choiceId}})
    }) }

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
  })
}

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

var router = new VueRouter({
  routes: [ { path: '/',            component: { props: ['choice'], template: '<p> <em> Click on or create a choice... </em> </p>' } },
            { path: '/choices/:id', component: routerComponent } ]
});

var app = new Vue({
  el:     '#app',
  router: router,
  data:   { choices: [], choice: null, errors: [] }
});

getChoices_();
