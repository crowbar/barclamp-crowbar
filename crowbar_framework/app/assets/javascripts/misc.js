//= require misc/format
//= require misc/arrayRemove
//= require misc/stringLocalize
//= require misc/localizedValue
//= require misc/iefix
//= require misc/handlebars

$(document).ready(function($) {
  Handlebars.registerHelper('toString', function (x) {
    return (x === undefined) ? 'undefined' : x.toString();
  });
});
