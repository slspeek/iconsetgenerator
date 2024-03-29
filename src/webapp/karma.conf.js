// Karma configuration
/* global basePath:true,
         files:true,
         exclude:true,
         autoWatch:true,
         browsers:true,
         captureTimeout:true,
         singleRun:true,
         reporters:true,
         runnerPort:true,
         JASMINE:false,
         JASMINE_ADAPTER:false,
         LOG_DEBUG:false,
         port:true,
         colors:true,
         logLevel:true
         */
// base path, that will be used to resolve files and exclude
basePath = '';

// list of files / patterns to load in the browser
files = [
  JASMINE,
  JASMINE_ADAPTER,
  'app/components/angular/angular.js',
  'app/components/angular-resource/angular-resource.js',
  'app/components/angular/',
  'app/components/angular-mocks/angular-mocks.js',
  'app/components/angular-bootstrap/ui-bootstrap-tpls.min.js',
  'app/components/angular-ui-utils/modules/keypress/keypress.js',
  'app/components/jquery/jquery.js',
  'app/components/jquery-ui/ui/jquery-ui.js',
  'app/components/jquery.colorpicker/jquery.colorpicker.js',
  'app/scripts/*.js',
  'app/scripts/**/*.js',
  'test/mock/**/*.js',
  'test/spec/**/*.js'
];

// list of files to exclude
exclude = [];

// test results reporter to use
// possible values: dots || progress || growl
reporters = ['progress'];

// web server port
port = 8080;

// cli runner port
runnerPort = 9100;

// enable / disable colors in the output (reporters and logs)
colors = true;

// level of logging
// possible values: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
logLevel = LOG_DEBUG;

// enable / disable watching file and executing tests whenever any file changes
autoWatch = false;

// Start these browsers, currently available:
// - Chrome
// - ChromeCanary
// - Firefox
// - Opera
// - Safari (only Mac)
// - PhantomJS
// - IE (only Windows)
browsers = ['Firefox'];

// If browser does not capture in given timeout [ms], kill it
captureTimeout = 5000;

// Continuous Integration mode
// if true, it capture browsers, run tests and exit
singleRun = false;
