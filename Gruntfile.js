module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 
    
        clean: ["externs", "js"],
        
        "purescript-make": {
            options: {
                tco: true,
                magicDo: true
            },
            lib: {
                options: { make: true },
                files: { _: ["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"] }
            }
        },
    
        purescript: {
            options: {
                main: true
            },
            exampleReader: {
                files: {
                    "js/_examples/Reader.js": ["examples/Reader.purs", "src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
                }
            },
            exampleState: {
                files: {
                    "js/_examples/State.js": ["examples/State.purs", "src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
                }
            },
            exampleWriter: {
                files: {
                    "js/_examples/Writer.js": ["examples/Writer.purs", "src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
                }
            },
            exampleCont: {
                files: {
                    "js/_examples/Cont.js": ["examples/Cont.purs", "src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
                }
            },
            exampleBounce: {
                files: {
                    "js/_examples/Bounce.js": ["examples/Bounce.purs", "src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
                }
            }
        }
        
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["purescript-make:lib"]);
};
