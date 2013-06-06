'use strict';

angular.module('iconApp', ['ngResource', 'ui.bootstrap', 'ui.keypress'])
  .config(function($routeProvider) {
    $routeProvider
      .when('/:IconName/:MainColor/:BgColor/:LineColor/:Shadow/:Background', {
        templateUrl: 'views/main.html',
        controller: 'MainCtrl'
      })
      .otherwise({
        redirectTo: 'overview/0077FD/F011DD/A20099/true/true'
      });
  });
