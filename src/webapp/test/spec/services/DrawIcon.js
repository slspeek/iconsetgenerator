'use strict';

describe('Service: DrawIcon', function () {

  // load the service's module
  beforeEach(module('iconApp'));

  // instantiate service
  var httpBackend, scope, DrawIcon;

  beforeEach(function(){
    this.addMatchers({
    toEqualData: function(expected) {
      return angular.equals(this.actual, expected);
      }
    });
  });

  beforeEach(inject(function ($injector, $httpBackend, $rootScope) {
    scope = $rootScope.$new();
    httpBackend = $httpBackend;
    httpBackend.expectGET('/icongenerator').respond({'url':'icon'});
    DrawIcon = $injector.get('DrawIcon');
  }));

  it('should return [{url:"icon"}]', function () {
		var result = DrawIcon.get();
		httpBackend.flush();
		expect(result).toEqualData({url:'icon'});
  });
});

