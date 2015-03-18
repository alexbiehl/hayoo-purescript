module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      options: {
        main: "Main",
        modules: ["Main"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"],

    browserify: {
      js: {
        src: "dist/Main.js",
        dest: "bundle.js"
      }
    },

    closureCompiler: {
      options: {
        compilerFile: 'bower_components/closure-compiler/compiler.jar',

        compilerOpts: {
         language_in: "ECMASCRIPT5_STRICT",
         compilation_level: 'ADVANCED_OPTIMIZATIONS',
         summary_detail_level: 3,
        },
      },
      bundle: {
        src: "bundle.js",
        dest: "bundle.min.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks('grunt-browserify');
  grunt.loadNpmTasks('grunt-closure-tools');

  grunt.registerTask("default", ["psc:all", "dotPsci", "browserify", "closureCompiler"]);
};
