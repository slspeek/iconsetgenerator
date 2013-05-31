'use strict';

describe('Controller: MainCtrl', function () {

  // load the controller's module
  beforeEach(module('iconApp'));

  var MainCtrl, $httpBackend, scope;

  beforeEach(function(){
    this.addMatchers({
    toEqualData: function(expected) {
      return angular.equals(this.actual, expected);
      }
    });
  });

  beforeEach(inject(function ($httpBackend, $rootScope) {
    scope = $rootScope.$new();
    $httpBackend.when('GET', '/iconlist').respond([{'name':'icon'}]);
  }));

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    MainCtrl = $controller('MainCtrl', {
      $scope: scope
    });
  }));

  it('should set bgColor', function () {
    expect(scope.bgColor).not.toBeUndefined();
  });
});
