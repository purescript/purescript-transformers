module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 
    
        clean: ["externs", "js"],
    
        purescript: {
            options: {
                tco: true,
                magicDo: true
            },
            lib: {
                options: { make: true },
                files: { _: ["src/**/*.purs"] }
            },
            exampleReader: {
                files: {
                    "js/_examples/Reader.js": ["examples/Reader.purs", "src/**/*.purs"]
                }
            },
            exampleState: {
                files: {
                    "js/_examples/State.js": ["examples/State.purs", "src/**/*.purs"]
                }
            },
            exampleWriter: {
                files: {
                    "js/_examples/Writer.js": ["examples/Writer.purs", "src/**/*.purs"]
                }
            }
        }
        
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["purescript:lib"]);
};