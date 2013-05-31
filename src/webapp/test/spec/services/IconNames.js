'use strict';

describe('Service: IconNames', function () {

  // load the service's module
  beforeEach(module('iconApp'));

  // instantiate service
  var $httpBackend, scope, IconNames;

  beforeEach(function(){
    this.addMatchers({
    toEqualData: function(expected) {
      return angular.equals(this.actual, expected);
      }
    });
  });

  beforeEach(inject(function ($injector, $rootScope) {
    scope = $rootScope.$new();
    $httpBackend = $injector.get('$httpBackend');
    $httpBackend.when('GET', '/iconlist').respond([{'name':'icon'}]);
    IconNames = $injector.get('IconNames');
  }));

  it('should return [{name:"icon"}]', function () {
    var iconList;
    IconNames.list(function(data) {
      iconList = data;
    });
    $httpBackend.flush();
    expect(iconList).toEqualData(['icon']);
  });
});
