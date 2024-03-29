// Karma E2E configuration
/* global basePath:true,
         files:true,
         exclude:true,
         autoWatch:true,
         browsers:true,
         captureTimeout:true,
         singleRun:true,
         reporters:true,
         runnerPort:true,
         ANGULAR_SCENARIO:false,
         ANGULAR_SCENARIO_ADAPTER:false,
         LOG_INFO:false,
         port:true,
         colors:true,
         proxies:true,
         logLevel:true
         */

// base path, that will be used to resolve files and exclude
basePath = '';

// list of files / patterns to load in the browser
files = [
  ANGULAR_SCENARIO,
  ANGULAR_SCENARIO_ADAPTER,
  'test/e2e/**/*.js'
];

// list of files to exclude
exclude = [];

// test results reporter to use
// possible values: dots || progress || growl
reporters = ['progress'];

// web server port
port = 8000;

proxies = {
  '/app/': 'http://localhost:8000/'
};
// cli runner port
runnerPort = 9100;

// enable / disable colors in the output (reporters and logs)
colors = true;

// level of logging
// possible values: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
logLevel = LOG_INFO;

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
