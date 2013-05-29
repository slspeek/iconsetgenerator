'use strict';

describe('Service: Icons', function () {

  // load the service's module
  beforeEach(module('iconApp'));

  // instantiate service
  var $httpBackend, scope, Icons;

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
    Icons = $injector.get('Icons');
  }));

  it('should return ["icon"]', function () {
    var iconList = Icons.query();
    expect(iconList).toEqual([]);
    $httpBackend.flush();
    expect(iconList).toEqualData([{'name':'icon'}]);
  });

});
