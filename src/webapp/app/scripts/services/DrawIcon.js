'use strict';

angular.module('iconApp')
  .factory('DrawIcon', function ($resource) {
        return $resource('icongenerator');
      });

