'use strict';

angular.module('iconApp', ['ngResource','ui.bootstrap', 'ui.keypress'])
  .config(function ($routeProvider) {
    $routeProvider
      .when('/designer/:IconName/:MainColor/:BgColor/:LineColor/:Background/:Shadow', {
        templateUrl: 'views/main.html',
        controller: 'MainCtrl'
      })
      .otherwise({
        redirectTo: 'designer/overview/0077FD/F011DD/A20099/true/true'
      });
  });
