'use strict';

describe('Controller: IconCtrl', function() {

    // load the controller's module
    beforeEach(module('iconApp'));

    var IconCtrl, $httpBackend,
      scope;

    beforeEach(inject(function($injector) {
          $httpBackend = $injector.get('$httpBackend');
          $httpBackend.when('GET', '/iconlist').respond([{
                'name': 'icon'
              }
            ]);
        }));

    // Initialize the controller and a mock scope
    beforeEach(inject(function($controller, $rootScope) {
          scope = $rootScope.$new();
          IconCtrl = $controller('IconCtrl', {
              $scope: scope
            });
        }));

    it('what is null', function() {
        var undef;
        if (undef != null) {
          fail();
        }
        if (!(undef === undefined || undef === null)) {
          fail();
        }
        undef = {};
        if (undef != null) {} else {

          fail();
        }
        if (!(undef === undefined || undef === null)) {} else {
          fail();
        }
      });
  });
