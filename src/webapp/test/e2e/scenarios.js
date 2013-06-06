/* http://docs.angularjs.org/guide/dev_guide.e2e-testing */

describe('Iconset Generator ', function() {

  beforeEach(function() {
    browser().navigateTo('/app/');
  });


  it('should automatically redirect to /view1 when location hash/fragment is empty', function() {
    expect(browser().location().url()).toBe('/overview/0077FD/F011DD/A20099/true/true');
  });


  describe('shadow checkbox', function() {

    beforeEach(function() {
    });


    it('should render view1 when user navigates to /view1', function() {
				input('shadow').check(false);
				element('#save').click();
    		expect(browser().location().url()).toBe('/overview/0077FD/F011DD/A20099/false/true');
    });

  });

});
