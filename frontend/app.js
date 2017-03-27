
function comp(name, props) {
  props.template = '#' + name;
  Vue.component(name, props);
}

function getChoice() {
  getChoicesByChoiceId(this.choice.choiceId, function(x){
    app.choice = x;
  })
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

// data Decision = Decision
//   { decisionChoiceId :: ID
//   , decisionId :: Maybe ID
//   , decision :: Option
//   } deriving (Eq, Show, Generic)

function decide() {
  var choiceId = this.option.optionChoiceId;
  var option   = this.option;
  console.log(choiceId);
  console.log(option);
  postChoicesByChoiceIdChoose(
    choiceId,
    option.optionId,
    function() {
      getChoice.call({choice: {choiceId: choiceId}})
    })}

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

var app = new Vue({
  el: '#app',
  data: { choices: [], choice: null }
})

function getChoices_() {
  getChoices(function(cs){
    app.choices = cs;
  })
}

getChoices_();
