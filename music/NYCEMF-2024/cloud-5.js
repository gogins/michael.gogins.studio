/**
 * Defines custom HTML elements for the cloud-5 system. Once a custom element 
 * is defined in the body of the HTML, its DOM object can be obtained, and 
 * then not only DOM methods but also custom methods can be called.
 * 
 * In general, rather than subclassing these custom elements  (although that 
 * is possible), users should define and set hook functions, code text, and 
 * other properties of the custom elements.
 * 
 * To simplify both usage and maintenance, internal styles are usually not 
 * used.
 * 
 * Usage:
 * 1. Include the csound-5.js script in the Web page.
 * 2. Lay out and style the required custom elements as with any other HTNL 
 *    elements. The w3.css is used internally and should also be used 
 *    externally.
 * 3. In a script element of the Web page:
 *    a. Define hook functions and code text as JavaScript variables.
 *    b. Obtain DOM objects from custom elements.
 *    c. Assign hook functions, code text, and DOM objects to their 
 *       relevant targets.
 */

/**
 * Sets up the piece, and defines menu items as required. The overlays are 
 * other custom elements.
 */
class Cloud5Piece extends HTMLElement {
  constructor() {
    super();
    this.csound = null;
    /**
     * May be assigned the text of a Csound .csd patch. If so, the Csound 
     * patch will be compiled and run for every performance. If the patch is 
     * null, then Csound will not be used.
     */
    this.csound_code_addon = null;
    /**
     * May be assigned a JavaScript object consisting of Csound control 
     * parameters, with default values. The naming convention must be global 
     * Csound variable type, underscore{ , Csound instrument name}, 
     * underscore, Csound control channel name. For example:
     * 
     * control_parameters_addon = {
     *  "gk_Duration_factor": 0.8682696259761612,
     *  "gk_Iterations": 4,
     *  "gk_MasterOutput_level": -2.383888203863542,
     *  "gk_Shimmer_wetDry": 0.06843403205918619
     * };
     *
     * The Csound orchestra should define matching control channels. Such 
     * parameters may also be used to control other processes.
     */
    this.control_parameters_addon = null;
    /**
     * May be assigned a score generating function. If so, the score generator 
     * will be called for each performance, and must generate and return a 
     * CsoundAC Score, which will be translated to a Csound score in text 
     * format, appended to the Csound patch, and played or rendered by Csound.
     */
    this.score_generator_function_addon = null;
    /**
     * May be assigned an instance of a cloud5-piano-roll overlay. If so, then 
     * the Score button will be created for showing and hiding a piano roll 
     * display of the generated CsoundAC Score.
     */
    this.piano_roll_overlay = null;
    /**
     * May be assigned an instance of the cloud5-log overlay. If so, 
     * the Log button will be created for showing and hiding a scrolling 
     * view of messages from Csound or other sources.
     */
    this.log_overlay = null;
    /**
     * May be assigned an instance of the cloud5-about overlay. If so,
     * the About button wll be created for showing and hiding the text 
     * of the overlay. The inner HTML of this element should contain 
     * license information, authorship, credits, and program notes for the 
     * piece.
     */
    this.about_overlay = null;
    /**
     * Metadata to be written to output files.
     */
    this.metadata = {
      "artist": null,
      "copyright": null,
      "performer": null,
      "title": null,
      "album": null,
      "track": null,
      "tracknumber": null,
      "date": null,
      "publisher": null,
      "comment": null,
      "license": null,
      "genre": null,
    };
  }
  /**
   * May be assigned an instance of a cloud5-shader overlay. If so, 
   * the GLSL shader will run at all times, and will normally create the 
   * background for other overlays. The shader overlay may call 
   * a hook function either to visualize the audio of the performance, 
   * or to sample the video canvas to generate notes for performance by 
   * Csound.
   */
  set shader_overlay(shader) {
    this.shader_overlay_ = shader;
    this.show(this.shader_overlay_);
  }
  /**
    * Called by the browser whenever this element is added to the 
    * document. Construction and initialization of the element should take 
    * place here.
    */
  connectedCallback() {
    this.innerHTML = `
    <div class="w3-bar" id="main_menu" style="position:fixed;background:transparent;z-index:10000;">
    <ul class="menu">
        <li id="menu_item_play" title="Play piece on system audio output" class="w3-btn w3-hover-text-light-green">
            Play</li>
        <li id="menu_item_render" title="Render piece to soundfile" class="w3-btn w3-hover-text-light-green">Render
        </li>
        <li id="menu_item_stop" title="Stop performance" class="w3-btn w3-hover-text-light-green">Stop</li>
        <li id="menu_item_fullscreen" class="w3-btn w3-hover-text-light-green">Fullscreen</li>
        <li id="menu_item_strudel" class="w3-btn w3-hover-text-light-green">Strudel</li>
        <li id="menu_item_piano_roll" title="Show/hide piano roll score" class="w3-btn w3-hover-text-light-green">Score
        </li>
        <li id="menu_item_log" title="Show/hide message log" class="w3-btn w3-hover-text-light-green">Log
        </li>
        <li id="menu_item_about" title="Show/hide information about this piece"
            class="w3-btn w3-hover-text-light-green">About</li>
        <li id="mini_console" class="w3-btn w3-text-green w3-hover-text-light-green"></li>
        <li id="vu_meter_left" class="w3-btn w3-hover-text-light-green"></li>
        <li id="vu_meter_right" class="w3-btn w3-hover-text-light-green"></li>
        <li id="menu_item_dat_gui"
            title="Show/hide performance controls; 'Save' copies all control parameters to system clipboard"
            class="w3-btn w3-left-align w3-hover-text-light-green w3-right"></li>
    </ul>
</div>`;
    // Save `this` for use in async "member functions."
    let host = this;
    this.vu_meter_left = document.querySelector("#vu_mter_left");
    this.vu_meter_right = document.querySelector("#vu_mter_right");
    this.mini_console = document.querySelector("#mini_console");
    this.csound_message_callback = async function (message) {
      if (message === null) {
        return;
      }
      let level_left = -100;
      let level_right = -100;
      if (non_csound(host.csound) == false) {
        let score_time = await host.csound.GetScoreTime();
        level_left = await host.csound.GetControlChannel("gk_MasterOutput_output_level_left");
        level_right = await host.csound.GetControlChannel("gk_MasterOutput_output_level_right");
        let delta = score_time;
        // calculate (and subtract) whole days
        let days = Math.floor(delta / 86400);
        delta -= days * 86400;
        // calculate (and subtract) whole hours
        let hours = Math.floor(delta / 3600) % 24;
        delta -= hours * 3600;
        // calculate (and subtract) whole minutes
        let minutes = Math.floor(delta / 60) % 60;
        delta -= minutes * 60;
        // what's left is seconds
        let seconds = delta % 60;  // in theory the modulus is not required
        if (level_left > 0) {
          $("#vu_meter_left").css("color", "red");
        } else if (level_left > -12) {
          $("#vu_meter_left").css("color", "orange")
        } else {
          $("#vu_meter_left").css("color", "lightgreen");
        }
        if (level_right > 0) {
          $("#vu_meter_right").css("color", "red");
        } else if (level_right > -12) {
          $("#vu_meter_right").css("color", "orange")
        } else {
          $("#vu_meter_right").css("color", "lightgreen");
        }
        $("#mini_console").html(sprintf("d:%4d h:%02d m:%02d s:%06.3f", days, hours, minutes, seconds));
        $("#vu_meter_left").html(sprintf("L%+7.1f dBA", level_right));
        $("#vu_meter_right").html(sprintf("R%+7.1f dBA", level_right));
      };
      console.log(message);
      host.log_overlay?.log(message);
    }
    const csound_message_callback_closure = function (message) {
      host.csound_message_callback(message);
    }
    //get_csound(csound_message_callback_closure);
    let menu_item_play = document.querySelector('#menu_item_play');
    menu_item_play.onclick = function (event) {
      console.log("menu_item_play click...");
      host.show(host.piano_roll_overlay)
      host.hide(host.strudel_overlay);
      // host.hide(host.shader_overlay);
      host.hide(host.log_overlay);
      host.hide(host.about_overlay);
      host.render(false);
    };
    let menu_item_render = document.querySelector('#menu_item_render');
    menu_item_render.onclick = function (event) {
      console.log("menu_item_render click...");
      host.show(host.piano_roll_overlay)
      host.hide(host.strudel_overlay);
      // host.hide(host.shader_overlay);
      host.hide(host.log_overlay);
      host.hide(host.about_overlay);
      host.render(true);
    };
    let menu_item_stop = document.querySelector('#menu_item_stop');
    menu_item_stop.onclick = function (event) {
      console.log("menu_item_stop click...");
      host.stop();
    };
    let menu_item_fullscreen = document.querySelector('#menu_item_fullscreen');
    menu_item_fullscreen.onclick = function (event) {
      console.log("menu_item_fullscreen click...");
      if (host.piano_roll_overlay.requestFullscreen) {
        host.piano_roll_overlay.requestFullscreen();
      } else if (host.piano_roll_overlay.webkitRequestFullscreen) {
        host.piano_roll_overlay.webkitRequestFullscreen();
      } else if (elem.msRequestFullscreen) {
        host.piano_roll_overlay.msRequestFullscreen();
      }
    };
    let menu_item_strudel = document.querySelector('#menu_item_strudel');
    menu_item_strudel.onclick = function (event) {
      console.log("menu_item_strudel click...");
      host.hide(host.piano_roll_overlay)
      host.toggle(host.strudel_overlay);
      // host.hide(host.shader_overlay);
      // host.hide(host.log_overlay);
      host.hide(host.about_overlay);
    };
    let menu_item_piano_roll = document.querySelector('#menu_item_piano_roll');
    menu_item_piano_roll.onclick = function (event) {
      console.log("menu_item_piano_roll click...");
      host.toggle(host.piano_roll_overlay)
      host.hide(host.strudel_overlay);
      // host.hide(host.shader_overlay);
      // host.hide(host.log_overlay);
      host.hide(host.about_overlay);
    };
    let menu_item_log = document.querySelector('#menu_item_log');
    menu_item_log.onclick = function (event) {
      console.log("menu_item_log click...");
      //host.show(host.piano_roll_overlay)
      host.hide(host.strudel_overlay);
      //host.hide(host.shader_overlay);
      host.toggle(host.log_overlay);
      host.hide(host.about_overlay);
    };
    let menu_item_about = document.querySelector('#menu_item_about');
    menu_item_about.onclick = function (event) {
      console.log("menu_item_about click...");
      host.hide(host.piano_roll_overlay)
      host.hide(host.strudel_overlay);
      ///host.hide(host.shader_overlay);
      host.hide(host.log_overlay);
      host.toggle(host.about_overlay);
    };
    // Ensure that the dat.gui controls are children of the _Controls_ button.
    let dat_gui_parameters = { autoPlace: false, closeOnTop: true, closed: true, width: 400, useLocalStorage: false };
    this.gui = new dat.GUI(dat_gui_parameters);
    let dat_gui = document.getElementById('menu_item_dat_gui');
    dat_gui.appendChild(this.gui.domElement);
    document.addEventListener("keydown", function (e) {
      let e_char = String.fromCharCode(e.keyCode || e.charCode);
      if (e.ctrlKey === true) {
        if (e_char === 'H') {
          var console = document.getElementById("console");
          if (console.style.display === "none") {
            console.style.display = "block";
          } else {
            console.style.display = "none";
          }
          this.gui.closed = true;
          gui.closed = false;
        } else if (e_char === 'G') {
          generate_score_hook();
        } else if (e_char === 'P') {
          parameters.play();
        } else if (e_char === 'S') {
          parameters.stop();
        }
      }
    });

    window.addEventListener("unload", function (event) {
      nw_window?.close();
    });

    // Polyfill to make 'render' behave like an async member function.
    this.render = async function (is_offline) {
      host.csound = await get_csound(host.csound_message_callback);
      if (non_csound(host.csound)) {
        return;
      }
      for (const key in host.metadata) {
        const value = host.metadata[key];
        if (value !== null) {
          host.csound.setMetadata(key, value);
        }
      }
      let csd = host.csound_code_addon.slice();
      let score = await host?.score_generator_function_addon();
      if (score) {
        let csound_score = await score.getCsoundScore(12., false);
        csound_score = csound_score.concat("\n</CsScore>");
        csd = host.csound_code_addon.replace("</CsScore>", csound_score);
      }
      host?.log_overlay.clear();
      if (is_offline == true) {
        csd = csd.replace("-odac", "-o" + document.title + ".wav");
      }
      // Save the .csd file so we can debug a failing orchestra,
      // instead of it just nullifying Csound.        
      const csd_filename = document.title + '-generated.csd';
      write_file(csd_filename, csd);
      let result = await host.csound.CompileCsdText(csd);
      host.csound_message_callback("CompileCsdText returned: " + result + "\n");
      await host.csound.Start();
      // Send _current_ dat.gui parameter values to Csound 
      // before actually performing.
      host.send_parameters(host.control_parameters_addon);
      host.csound_message_callback("Csound has started...\n");
      if (is_offline == false) {
        await host.csound.Perform();
        console.log("strudel_view:", host.strudel_view);
        strudel_view?.setCsound(host.csound);
        strudel_view?.startPlaying();

      } else {
        // Returns before finishing because Csound will perform in a separate 
        // thread.
        await host.csound.performAndPostProcess();
      }
      host.piano_roll_overlay?.trackScoreTime();
      host?.csound_message_callback("Csound is playing...\n");
    }
    this.stop = async function () {
      this.piano_roll_overlay?.stop();
      await host.csound.Stop();
      await host.csound.Cleanup();
      host.csound.Reset();
      strudel_view?.stopPlaying();
      host.csound_message_callback("Csound has stopped.\n");
    }
  }
  show(overlay) {
    if (overlay) {
      overlay.style.display = 'block';
    }
  }
  hide(overlay) {
    if (overlay) {
      overlay.style.display = 'none';
    }
  }
  toggle(overlay) {
    if (overlay) {
      if (overlay.checkVisibility() == true) {
        this.hide(overlay);
      } else {
        this.show(overlay);
      }
    }
  }
  /**
   * Sends the values of the parameters to the Csound control channels 
   * with the same names.
   */
  send_parameters(parameters_) {
    if (non_csound(this.csound) == false) {
      for (const [name, value] of Object.entries(parameters_)) {
        this.csound.Message(name + ": " + value + "\n");
        this.csound.SetControlChannel(name, parseFloat(value));
      }
    }
  }
  controls_add_folder(name) {
    let folder = this.gui.addFolder(name);
    return folder;
  }
  controls_add_slider(gui_folder, token, minimum, maximum, step) {
    const on_parameter_change = function (value) {
      host.gk_update(token, value);
    };
    gui_folder.add(this.control_parameters_addon, token, minimum, maximum, step).listen().onChange(on_parameter_change);
    // Remembers parameter values. Required for the 'Revert' button to 
    // work, and to be able to save/restore new presets.
    this.gui.remember(this.control_parameters_addon);
  }
  gk_update(name, value) {
    const numberValue = parseFloat(value);
    console.log("gk_update: name: " + name + " value: " + numberValue);
    if (non_csound(this.csound) == false) {
      this.csound.SetControlChannel(name, numberValue);
    }
  }
  menu_add_command(control_parameters_addon, gui_folder, name, onclick) {
    control_parameters_addon['name'] = onclick;
    gui_folder.add(this.control_parameters_addon, name)
  }
}
customElements.define("cloud5-piece", Cloud5Piece);

/**
 * Displays a CsoundAC Score as a 3-dimensional piano roll.
 */
class Cloud5PianoRoll extends HTMLElement {
  constructor() {
    super();
    this.csound5_piece = null;
    this.silencio_score = new Silencio.Score();
    this.csoundac_score = null;
    this.canvas = null;
    this.interval_id = null;
  }
  /**
    * Called by the browser whenever this element is added to the document.
    */
  connectedCallback() {
    this.style.background='black';
    this.innerHTML = `
     <canvas id="display" class='cloud5-panel' style='background:black;z-index:100;'>
    `;
    this.canvas = this.querySelector('#display');
    if (this.csoundac_score !== null) {
      this.draw(this.csoundac_score);
    }
  }
  draw(csoundac_score_) {
    this.csoundac_score = csoundac_score_;
    this.silencio_score.copyCsoundAcScore(this.csoundac_score);
    this.silencio_score.draw3D(this.canvas);
  }
  trackScoreTime() {
    const host = this;
    const trackScoreTime_ = function () {
      if (non_csound(this.csound5_piece.csound)) {
        return;
      }
      let score_time = this.csound5_piece.csound.getScoreTime();
      this.silencio_score.progress3D(score_time);
    }
    this.interval_id = setInterval(trackScoreTime_.bind(this), 200);
  }
  stop() {
    clearInterval(this.interval_id);
  }
}
customElements.define("cloud5-piano-roll", Cloud5PianoRoll);

/**
 * Contains an instance of the Strudel REPL that can use Csound as an output,
 * and starts and stops along wth Csound.
 */
class Cloud5Strudel extends HTMLElement {
  constructor() {
    super();
    this.strudel_code_ = null;
  }
  /**
    * Called by the browser whenever this element is added to the document.
    */
  connectedCallback() {
    this.innerHTML = `
    <strudel-repl-component id="strudel_view" 
        style="position:absolute;left:70px;top:80px;z-index:1;">

        <!--
        ${this.strudel_code_}
        -->
    </strudel-repl-component>
    `;
    this.strudel_component = this.querySelector('#strudel_view');

  }
  start() {
    this.strudel_component.startPlaying();

  }
  stop() {
    this.strudel_component.stopPlaying();

  }
  set strudel_code_addon(code) {
    this.strudel_code_ = code;
    // Reconstruct the element.
    this.connectedCallback();
  }
}
customElements.define("cloud5-strudel", Cloud5Strudel);

/**
 * Presents visuals generated by a GLSL shader. These visuals can 
 * show a visualization of the music, or be sampled to generate notes for 
 * Csound to perform.
 * 
 * The SWSS glsl function accepts a dictionary of parameters, documented 
 * here: https://github.com/google/swissgl/blob/main/docs/API.md. The most 
 * often used of these parameters have setters in this class.
 */
class Cloud5Shader extends HTMLElement {
  constructor() {
    super();
    /**
     * The user may define a vertex shader in GLSL code for generating 
     * visuals, and assign a string containing the code to this property.
     */
    this.glsl_parameters = {};
    /**
     * This is GLSL code for a default vertex shader; the user may define a 
     * different vertex shader and assign it to this property.
     */
    this.vertex_shader_code_addon = `#version 300 es
    in vec2 inPos;
    void main() {
        gl_Position = vec4(inPos.xy, 0.0, 1.0);
    }`;
    /**
     * The user may define a function that will be called at intervals to 
     * receive a real-time FFT analysis of the audio; the function should 
     * downsample and/or otherwise process the analysis to generate CsoundAC 
     * Notes, which must be returned in a CsoundAC Score. The user-defined 
     * function must be assigned to this property.
     */
    this.shader_sampler_hook = null;
    /**
     * The user may define a function that will be called at intervals to 
     * receive an FFT analysis of the performance; the function should use 
     * these to compute GLSL uniforms that will in some way control the 
     * appearance and behavior of the shader visuals. The user-defined 
     * function must be assigned to this property.
     */
    this.audio_visualizer_hook = null;
  }
  set fragment_shader_code_addon(code) {
    this.glsl_parameters['FP'] = code;
  }
  set vertex_shader_code_addon(code) {
    this.glsl_parameters['VP'] = code;
  }
  set normal_addon(variable){
    this.glsl_parameters[variable] = variable;
  }
  set included_shader_code_addon(code) {
    this.glsl_parameters['Inc'] = code;
  }
  /**
    * Called by the browser whenever this element is added to the document.
    */
  connectedCallback() {
    this.canvas = document.createElement('canvas');
    this.appendChild(this.canvas);
    this.canvas.style.position = 'absolute';
    this.canvas.style.top = '0';
    this.canvas.style.left = '0';
    this.canvas.style.margin_top = '40px';
    this.canvas.style.display='block';
    this.canvas.style.width='100%';
    this.canvas.style.height='100%';
    //this.canvas.style.zIndex = '0';
    this.glsl = SwissGL(this.canvas);
    let host = this;
    let render = function (t) {
      t /= 1000; // ms to sec
      host.glsl({
        t, // pass uniform 't' to GLSL
        Mesh: [100, 100],  // draw a 10x10 tessellated plane mesh
        // Vertex shader expression returns vec4 vertex position in
        // WebGL clip space. 'XY' and 'UV' are vec2 input vertex 
        // coordinates in [-1,1] and [0,1] ranges.
        VP: `XY*0.8+sin(t+XY.yx*2.0)*0.2,0,1`,
        // Fragment shader returns 'RGBA'
        FP: `UV,0.5,1`
      });
      requestAnimationFrame(render);
    }
    requestAnimationFrame(render);
  }
}
customElements.define("cloud5-shader", Cloud5Shader);

/**
 * Displays a scrolling list of runtime messages from Csound and/or other 
 * sources.
 */
class Cloud5Log extends HTMLElement {
  constructor() {
    super();
    this.csound5_piece = null;
  }
  /**
    * Called by the browser whenever this element is added to the document.
    */
  connectedCallback() {
    this.innerHTML = `<div 
      id='console_view' 
      class="w3-text-sand cloud5-panel"
      style="background-color:transparent;z-index:4;opacity:60%">`;
    this.message_callback_buffer = "";
    this.console_editor = ace.edit("console_view");
    this.console_editor.setShowPrintMargin(false);
    this.console_editor.setDisplayIndentGuides(false);
    this.console_editor.renderer.setOption("showGutter", false);
    this.console_editor.renderer.setOption("showLineNumbers", true);
  };
  log(message) {
    // Split in case the newline is in the middle of the message but 
    // not at the end?
    this.message_callback_buffer = this.message_callback_buffer + message;
    if (this.message_callback_buffer.endsWith("\n")) {
      console.log(this.message_callback_buffer);
      let lines = this.console_editor.getSession().getLength();
      // Prevent the console editor from hogging memory.
      if (lines > 5000) {
        this.console_editor.getSession().removeFullLines(0, 2500);
        lines = this.console_editor.getSession().getLength();
      }
      this.console_editor.moveCursorTo(lines, 0);
      this.console_editor.scrollToLine(lines);
      this.console_editor.insert(this.message_callback_buffer);
      this.message_callback_buffer = "";
    };
  }
  clear() {
    this.console_editor.setValue('');
  }
};
customElements.define("cloud5-log", Cloud5Log);

/**
 * Contains license, authorship, credits, and program notes as the inner HTML 
 * of this.
 */
class Cloud5About extends HTMLElement {
  constructor() {
    super();
  }
}
customElements.define("cloud5-about", Cloud5About);

// A sad workaround....
try {
  var fs = require("fs");
  var __dirname = fs.realpathSync.native(".");
} catch (e) {
  console.log(e);
}

/**
 * The title of the piece is always the basename of the document.
 */
document.title = document.location.pathname.replace("/", "").replace(".html", "");

/**
 * Tries to clear all browser caches upon loading.
 */
if ('caches' in window) {
  caches.keys().then(function (names) {
    for (let name of names)
      caches.delete(name);
    console.log(`deleted ${name} from caches.`);
  });
}

/**
 * Tests if Csound is null or undefined.
 */
function non_csound(csound_) {
  if (typeof csound_ === 'undefined') {
    console.warn("csound is undefined.");
    console.trace();
    return true;
  }
  if (csound_ === null) {
    console.warn("csound is null.");
    console.trace();
    return true;
  }
  return false;
}

/**
 * Replaces the order of instruments in a CsoundAC Score with a new order.
 * Instrument numbers are re-ordered as if they are integers. The 
 * new_order parameter is a map, e.g. `{1:5, 3:1, 4:17}`. The map need not 
 * be complete.
 */
function arrange_silencio(score, new_order_) {
  console.log("arrange: reassigning instrument numbers...")
  let new_order = new Map(Object.entries(new_order_));
  // Renumber the insnos in the Score. Fractional parts of old insnos are 
  // preserved.
  for (i = 0, n = score.data.length; i < n; ++i) {
    let event_ = score.data[i];
    let current_insno = event_.channel;
    let current_insno_integer = Math.floor(current_insno);
    let string_key = current_insno_integer.toString();
    if (new_order.has(string_key)) {
      let new_insno_integer = new_order.get(string_key);
      let new_insno_fraction = current_insno - current_insno_integer;
      let new_insno = new_insno_integer + new_insno_fraction;
      console.log("renumbered: " + event_.toIStatement());
      event_.channel = new_insno;
      score.data[i] = event_;
      console.log("        to: " + score.data[i].toIStatement());
    }
  }
  console.log("arrange: finished reassigning instrument numbers.\n")
}

/**
 * Replaces the order of instruments in a CsoundAC Score with a new order.
 * Instrument numbers are re-ordered as if they are integers. The 
 * new_order parameter is a map, e.g. `{1:5, 3:1, 4:17}`. The map need not 
 * be complete.
 */
function arrange(score, new_order_) {
  console.log("arrange: reassigning instrument numbers...\n")
  let new_order = new Map(Object.entries(new_order_));
  // Renumber the insnos in the Score. Fractional parts of old insnos are 
  // preserved.
  for (i = 0, n = score.size(); i < n; ++i) {
    let event_ = score.get(i);
    let current_insno = event_.getInstrument();
    let current_insno_integer = Math.floor(current_insno);
    let string_key = current_insno_integer.toString();
    if (new_order.has(string_key)) {
      let new_insno_integer = new_order.get(string_key);
      let new_insno_fraction = current_insno - current_insno_integer;
      let new_insno = new_insno_integer + new_insno_fraction;
      console.log("renumbered: " + event_.toIStatement());
      event_.setInstrument(new_insno);
      score.set(i, event_);
      console.log("        to: " + event_.toIStatement());
    }
  }
  console.log("arrange: finished reassigning instrument numbers.\n")
}

function write_file(filepath, data) {
  var fs = require('fs');
  try {
    // Sync, so a bad .csd file doesn't blow up Csound 
    // before the .csd file is written so it can be tested!
    fs.writeFileSync(filepath, data, function (err) {
      console.error(err);
    });
  } catch (err) {
    console.warn(err);
  }
}

/**
 * Copies all _current_ dat.gui parameters to the system clipboard in 
 * JSON format.
 */
function copy_parameters(parameters) {
  const json_text = JSON.stringify(parameters, null, 4);
  navigator.clipboard.writeText(json_text);
  console.log("Copied all control parameters to system clipboard.\n")
}

