'use strict';

describe('Service: IconNames', function() {

    // load the service's module
    beforeEach(module('iconApp'));

    // instantiate service
    var $log, httpBackend, scope, IconNames;

    beforeEach(function() {
        this.addMatchers({
            toEqualData: function(expected) {
              return angular.equals(this.actual, expected);
            }
          });
      });

    beforeEach(inject(function($injector, $httpBackend, $rootScope) {
          scope = $rootScope.$new();
          httpBackend = $httpBackend;
          httpBackend.when('GET', '/iconlist').respond([{
                'name': 'icon'
              }
            ]);
          IconNames = $injector.get('IconNames');
        }));

    it('should return [{name:"icon"}]', function() {
        var iconList;
        IconNames.list().then(function(data) {
            iconList = data;
          });
        httpBackend.flush();
        expect(iconList).toEqualData(['icon']);
      });
  });

describe('function: toStringList', function() {
    it('should return an array of strings', function() {
        expect(toStringList({
              data: [{
                  name: 'Euclid'
                }
              ]
            })).toEqual(['Euclid']);
      });
  });
