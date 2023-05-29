/**
P I A N O   R O L L   3 D

Copyright (C) 2021 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of PianoRoll3D, an HTML5 algorithmic music composition library for Csound.

DEPENDENCIES

fs
three.js
TrackballControls.js
sprintf.js
tinycolor.js

*/

(function() {
    /**
     * Given either a score object or the JSON representation 
     * of a score object, displays a 3-dimensional, rotatable and 
     * zoomable piano roll view of the score in a Canvas element.
     * The score object is { events: [], minima: [], maxima [], ranges[] }; where 
     * events is an array of arrays of floating-point numbers with elements
     * [TIME, DURATION, STATUS, INSTRUMENT, KEY, VELOCITY, PHASE, DEPTH, PAN, HEIGHT, PCS, HOMOGENEITY]
     * just as in csound::Score.
     */
    class PianoRoll3D {
        constructor(canvas) {
            this.score = {};
            this.score_duration = 0;
            this.canvas = canvas;
            this.renderer = new THREE.WebGLRenderer({
                canvas: this.canvas,
                antialias: true
            });
            console.log("renderer: " + this.renderer);
        }
        time(event) {
            return event[0];
        }
        duration(event) {
            return event[1];
        }
        end(event) {
            return this.time(event) + this.duration(event);
        }
        status(event) {
            return event[2];
        }
        instrument(event) {
            return event[3];
        }
        key(event) {
            return event[4];
        }
        velocity(event) {
            return event[5];
        }
        fromJson(json_text) {
            this.score = JSON.parse(json_text);
            this.findDuration();
            this.draw3D(this.canvas);
        }
        fromObject(score_object) {
            this.score = score_object;
            this.findDuration();
            this.draw3D(this.canvas);
        }
        prepareScene3D(canvas) {
            this.canvas = canvas;
            canvas.width = canvas.clientWidth;
            canvas.height = canvas.clientHeight;
            this.scene = new THREE.Scene();
            let scene = this.scene;
            let renderer = this.renderer;
            renderer.setClearColor(0);
            renderer.sortObjects = false;
            renderer.setViewport(0, 0, canvas.clientWidth, canvas.clientHeight);
            renderer.setPixelRatio(canvas.devicePixelRatio);
            // Wire up the view controls to the camera.
            this.camera = new THREE.PerspectiveCamera(45, canvas.clientWidth / canvas.clientHeight, 1, 10000);
            let camera = this.camera;
            this.controls = new THREE.TrackballControls(camera, canvas);
            let controls = this.controls;
            controls.rotateSpeed = 1.0;
            controls.zoomSpeed = 1;
            controls.panSpeed = 1;
            controls.noZoom = false;
            controls.noPan = false;
            controls.staticMoving = true;
            controls.dynamicDampingFactor = 0.3;
            // Ensure that all sides are lighted.
            let light = new THREE.DirectionalLight(0xffffff, 1);
            light.position.set(1, 1, 1).normalize();
            this.scene.add(light);
            let light2 = new THREE.AmbientLight(0x404040, 0.5);
            this.scene.add(light2);
            let this_ = this;
            let onResize = function() {
                this_.canvas.width = this_.canvas.clientWidth;
                this_.canvas.height = this_.canvas.clientHeight;
                this_.renderer.setViewport(0, 0, this_.canvas.clientWidth, this_.canvas.clientHeight);
                this_.camera.aspect = this_.canvas.clientWidth / this_.canvas.clientHeight;
                this_.controls.handleResize();
                this_.camera.updateProjectionMatrix();
                this_.renderer.render(this_.scene, this_.camera);
            };
            window.addEventListener('resize', onResize, false);
        };
        /**
         * Adds the note to the 3D scene. 
         */
        plotNote3D(note) {
            let begin = this.time(note) - this.time(this.score.minima);
            let end = this.end(note) - this.time(this.score.minima);
            let duration = this.duration(note);
            let key = this.key(note);
            let channel = this.instrument(note) - this.instrument(this.score.minima);
            let geometry = new THREE.BoxBufferGeometry(duration, 1, 1);
            if (this.score.ranges[3] === 0) {
                this.score.ranges[3] = 1;
            }
            if (this.score.ranges[5] === 0) {
                this.score.ranges[5] = 1;
            }
            let hue = channel / this.score.ranges[3];
            let value = note[5] - this.score.minima[5];
            value = value / this.score.ranges[5];
            value = 0.5 + value / 2;
            let material = new THREE.MeshLambertMaterial();
            material.color.setHSL(hue, 1, value);
            material.opacity = 0.5;
            material.reflectivity = 0.5;
            material.transparent = true;
            material.emissive = material.color;
            material.emissiveIntensity = 2 / 3;
            let note_mesh = new THREE.Mesh(geometry, material);
            note_mesh.position.x = begin + duration;
            note_mesh.position.y = key;
            note_mesh.position.z = channel +.5;
            //console.log(`note_x: ${begin} note y: ${key} note z: ${channel}`);
            this.scene.add(note_mesh);
        }
        findDuration() {
            this.score_duration = 0;
            for (let i = 0; i < this.score.events.length; ++i) {
                let event = this.score.events[i];
                let event_off = this.end(event);
                if (event_off > this.score_duration) {
                    this.score_duration = event_off;
                }
            }
            console.log("PianoRoll3D: score_duration: " + this.score_duration);
        }
        /**
         * Plots a grid for a fixed score.
         */
        plotGrid3D() {
            // Generate the grid. Its origin for time is 0 and for pitch its origin is the
            // first C lower than or equal to the lowest pitch in the score.
            let time_minimum = this.time(this.score.minima);
            let time_maximum = time_minimum + this.score_duration;
            console.log("time_maximum: " + time_maximum);
            console.log("this.score_duration: " + this.score_duration);
            let key_minimum = this.key(this.score.minima);
            let key_maximum = this.key(this.score.maxima);
            let channel_minimum = this.instrument(this.score.minima);
            let channel_maximum = this.instrument(this.score.maxima);
            let line_material = new THREE.LineBasicMaterial();
            let instrument_minimum = 0;
            if (key_minimum % 12 !== 0) {
                key_minimum -= (key_minimum % 12);
            }
            let grid_geometry = new THREE.BoxBufferGeometry(10, 12, 1);
            for (let t = time_minimum; t <= (time_maximum + 10); t = t + 10) {
                for (let k = key_minimum; k <= key_maximum; k = k + 12) {
                    let box = new THREE.LineSegments(new THREE.EdgesGeometry(grid_geometry), line_material);
                    ///box = new THREE.EdgesGeometry(box);
                    box.material.color.setRGB(0, 0.25, 0);
                    box.material.opacity = 0.25;
                    box.material.transparent = true;
                    box.position.x = t + 5;
                    box.position.y = k + 6;
                    box.position.z = 0;
                    box.scale.z = 0;
                    this.scene.add(box);
                }
            }
            // Put a ball at the origin, to indicate the orientation of the score.
            let origin_geometry = new THREE.SphereGeometry(1, 10, 10);
            let origin_material = new THREE.MeshLambertMaterial();
            origin_material.color.setRGB(0, 255, 0);
            let origin = new THREE.Mesh(origin_geometry, origin_material);
            origin.position.x = time_minimum;
            origin.position.y = key_minimum;
            origin.position.z = 0;
            this.scene.add(origin);
            // Put a ball at the start of middle C, to indicate the current Csound
            // score time.
            let cursor_geometry = new THREE.SphereGeometry(1, 10, 10);
            let cursor_material = new THREE.MeshLambertMaterial();
            cursor_material.color.setRGB(255, 0, 0);
            this.score_cursor = new THREE.Mesh(cursor_geometry, cursor_material);
            this.score_cursor.position.x = time_minimum;
            this.score_cursor.position.y = 60;
            this.score_cursor.position.z = 0;
            this.scene.add(this.score_cursor);
        };
        /**
         * Looks at a full fixed score.
         */
        lookAtFullScore3D() {
            let bounding_box = new THREE.Box3().setFromObject(this.scene);
            this.camera.lookAt(bounding_box.getCenter());
            this.camera.fov = 2 * Math.atan((bounding_box.getSize().x / (this.canvas.width / this.canvas.height)) / (2 * bounding_box.getSize().y)) * (180 / Math.PI);
            this.camera.position.copy(bounding_box.getCenter());
            this.camera.position.z = 1.125 * Math.min(bounding_box.getSize().x, bounding_box.getSize().y);
            this.controls.target.copy(bounding_box.getCenter());
            this.controls.update();
            this.camera.updateProjectionMatrix();
            this.renderer.render(this.scene, this.camera);
        };
        /**
         * Looks at the front (current notes) of a real-time score.
         */
        lookAtFront3D() {
            let bounding_box = new THREE.Box3().setFromObject(this.scene);
            this.camera.lookAt(bounding_box.getCenter());
            this.camera.fov = 2 * Math.atan((bounding_box.getSize().y / (this.canvas.width / this.canvas.height)) / (2 * bounding_box.getSize().z)) * (180 / Math.PI);
            this.camera.position.copy(bounding_box.getCenter());
            this.camera.position.x = 1.125 * Math.max(bounding_box.getSize().x, bounding_box.getSize().y);
            this.controls.target.copy(bounding_box.getCenter());
            this.controls.update();
            this.camera.updateProjectionMatrix();
            this.renderer.render(this.scene, this.camera);
        };
        /**
         * Redraws the scene using the camera updated from the controls.
         */
        render3D() {
            this.controls.update();
            this.camera.updateProjectionMatrix();
            this.renderer.render(this.scene, this.camera);
        };
        /**
         * Draws the notes in a fixed score as a 3-dimensional piano roll. The score is
         * fitted into the viewport to start with, but the user can use the mouse or
         * trackball to move around the score and to zoom in and out. The dimensions
         * are: time = x, MIDI key = y, MIDI channel = z and hue, and loudness =
         * value; a grid shows tens of seconds and octaves.
         */
        draw3D(canvas) {
            this.prepareScene3D(canvas);
            this.plotGrid3D();
            // Plot the notes.
            for (let i = 0; i < this.score.events.length; i++) {
                this.plotNote3D(this.score.events[i]);//], this.score.minima[0], this.score.ranges[0], this.score.minima[5], this.score.ranges[5]);
            }
            this.lookAtFullScore3D();
            return canvas;
        };
        progress3D(score_time) {
            if (this.scene !== null) {
                this.score_cursor.position.x = score_time;
                this.score_cursor.position.y = 60;
                this.score_cursor.position.z = 0.5;
                this.controls.update();
                this.camera.updateProjectionMatrix();
                this.renderer.render(this.scene, this.camera);
            }
        };  
    };
    
    var PianoRoll = {
        PianoRoll3D: PianoRoll3D
    };
    // Node: Export function
    if (typeof module !== "undefined" && module.exports) {
        module.exports = PianoRoll;
    }
    // AMD/requirejs: Define the module
    else if (typeof define === 'function' && define.amd) {
        define(function() {
            return PianoRoll;
        });
    }
    // Browser: Expose to window
    else if (typeof window !== 'undefined') {
        window.PianoRoll = PianoRoll;
    } else {
        return PianoRoll;
    }

})();
    


