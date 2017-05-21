
// Set the XSRF Header correctly on each request...
// Powered by JQuery
//
$.ajaxPrefilter(function(opts, origOpts, xhr) {
  Cookies.raw = true;
  var token = Cookies.get('XSRF-TOKEN');
  if( token ) {
    xhr.setRequestHeader('X-XSRF-TOKEN', token);
  }
})

function comp(name, props) {
  props.template = '#' + name;
  Vue.component(name, props);
}

function getChoice() {
  getChoice_(app, this.choice.choiceId);
}

function getChoice_(o,i) {
  getChoicesByChoiceId(i,
    function(x){ o.choice = x; },
    function(x){ push_error("Couldn't fetch choice " + i); }
  ); }

function newChoice() {
  postChoices( {choiceName: this.choiceName},
    function(choiceResponse) { getChoices_() },
    function(x) { console.log(x); push_error("Could not create choice due to error "); }
  ) }

function newOption() {
  var choiceId = this.choice.choiceId;
  var optName  = this.optionName;
  if(! optName || optName == "") {
    push_error("Please fill in the option name ");
    return;
  }
  postChoicesByChoiceIdAdd(
    choiceId,
    { optionName: optName, optionChoiceId: choiceId},
    function(optionResponse) { getChoice.call({choice: {choiceId: choiceId}}); },
    function(err)            { console.log(err); push_error("Error adding option to choice "); }
  )}

function decide() {
  var choiceId = this.option.optionChoiceId;
  var option   = this.option;
  postChoicesByChoiceIdChoose(
    choiceId,
    option.optionId,
    function()    { getChoice.call({choice: {choiceId: choiceId}}) },
    function(err) { console.log(err); push_error("Couldn't decide due to an error "); }
  )}

function removeAllErrors() {
  app.errors = [];
}

function getChoices_() {
  getChoices(
    function(cs)  { app.choices = cs; },
    function(err) { console.log(err); push_error("Error Listing Choices "); }
  )}

function getUser_() {
  getMe(function(user) {
    console.log("Found user: " + user);
    app.user = user;
    getChoices_();
  }, function (err) {
    console.log("Couldn't automatically log in");
    console.log(err);
  })
}

function register(a) {
  postRegister(
    { username: this.username, password: this.password },
    function(res) { console.log(res); app.user = res; getChoices_(); },
    function(err) { console.log(err); push_error("Could not register"); }
  )
  console.log("Caught post register event");
  console.log(a);
  a.preventDefault();
  }

function login(a) {
  postLogin(
    { username: this.username, password: this.password },
    function(res) { console.log(res); app.user = res; getChoices_(); },
    function(err) { console.log(err); push_error("Could not log in "); }
  )
  console.log("Caught post login event");
  console.log(a);
  a.preventDefault();
  }

function logout() {
  function resetApp() {
    app.choices = [];
    app.choice  = null;
    app.errors  = [];
    app.user    = null;
    app.errors.push("Logged Out");
  }
  postLogout(
    function(res) { resetApp(); },
    function(err) { resetApp(); }
  ) }

function push_error(err) {
  console.log(err);
  app.errors.push(err);
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

comp('login-dialog', { props:    ['username', 'password'],
                       methods:  { login: login, register: register }});

comp('user-info',    { props:    ['user'],
                       methods:  { logout: logout }});

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
  data:   { choices: [], choice: null, errors: [], user: null }
});

getUser_();
