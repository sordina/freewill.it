
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

function newChoice(e) {
  var i = e.srcElement;
  if(! i.value || i.value == "") {
    push_error("Please fill in the choice name ");
    return
  }
  postChoices( {choiceName: i.value},
    function(choiceResponse) { i.value = ''; getChoices_() },
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

function ownedbyme() {
  return this.choice.choiceUserId == app.user.userId; // TODO: Shouldn't reference app explicitly
  }

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
  a.preventDefault();
  if(! this.username.match(/@/)) {
    push_error("Username must be an email address.");
    return;
  }
  postRegister(
    { username: this.username, password: this.password },
    function(res) { console.log(res); app.user = res; getChoices_(); },
    function(err) { console.log(err); push_error("Could not register"); }
  )
  }

function login(a) {
  a.preventDefault();
  if(! this.username.match(/@/)) {
    push_error("Username must be an email address.");
    return;
  }
  postLogin(
    { username: this.username, password: this.password },
    function(res) { console.log(res); app.user = res; getChoices_(); },
    function(err) { console.log(err); push_error("Could not log in "); }
  )
  }

function logout() {
  function resetApp() {
    app.choices = [];
    app.choice  = null;
    app.errors  = [];
    app.user    = null;
    router.push("/");
  }
  // TODO: Currently the response is an error, but we should change that on the backend...
  postLogout(
    function(res) { resetApp(); },
    function(err) { resetApp(); }
  ) }

function push_error(err) {
  console.log(err);
  app.errors.push(err);
}

// TODO: Improve this crap!
function myChoices() {
  return this.choices.filter(
    function(c) {
      return c.choiceUserId == app.user.userId;
    }).reverse();
}

function otherChoices() {
  return this.choices.filter(
    function(c) { return c.shared; }).reverse();
}

comp('choices-list', { props:    ['choices' ],
                       methods:  { newChoice: newChoice },
                       computed: { myChoices: myChoices, otherChoices: otherChoices }});

comp('choice-item',  { props:    ['choice'],
                       methods:  { clickChoice: getChoice } });

comp('choice-info',  { props:    ['choice', 'options', 'decision', 'optionName', 'shared'],
                       methods:  { newOption: newOption },
                       computed: {
                         revOptions: function() { return this.options.reverse() },
                         ownedbyme: ownedbyme,
                         shared: {
                           get: function () { return this.choice.shared; },
                           set: function (x) {
                             var c = this.choice;
                             var previousshared = c.shared;
                             c.shared = x;
                             // TODO: Use success and error callbacks when setting shared property
                             if(x) {
                               postChoicesByChoiceIdShare(c.choiceId,
                                 function(){ console.log("Shared"); },
                                 function(){ push_error("Couldn't share choice"); c.shared = previousshared; });
                             } else {
                               postChoicesByChoiceIdHide(c.choiceId,
                                 function(){ console.log("hidden"); },
                                 function(){ push_error("Couldn't hide choice"); c.shared = previousshared; });
                             }
                       } } } });

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

setInterval(function(){ if(app.user) { getChoices_(); }}, 30000);
