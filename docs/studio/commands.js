// The command sheet shown from the header. Same sentences and commands as
// studio-tools/docs/cheatsheet.md. This page only plays; the sheet is how
// to create and revise a piece in Cursor.

import { h } from './dom.js';

const SENTENCES = [
  ['New piece',
    'Follow studio-tools/AGENT_INSTRUCTIONS.md. Create a composition named NAME: …. Render, accept, publish, and push to master.'],
  ['Next version',
    'Follow studio-tools/AGENT_INSTRUCTIONS.md. Revise NAME: …. Render, accept, publish, and push to master.'],
];

const COMMANDS = [
  ['See what is installed', 'studio env'],
  ['New piece', 'studio new NAME'],
  ['Next version', 'studio stage NAME'],
  ['Save the prompt, word for word', 'studio prompt "$S" --file FILE'],
  ['Run a generator', 'studio run "$S" --seed N -- python code/NAME-generate.py'],
  ['Check, render, measure', 'studio render "$S"'],
  ['Keep this version', 'studio accept "$S" --model "…" --agent "…"'],
  ['Throw away the staging area', 'studio discard "$S"'],
  ['Copy it to the page', 'studio publish NAME'],
  ['Put it on GitHub', 'git push'],
  ['What exists', 'studio list'],
  ["One version's record", 'studio show NAME'],
  ['Check the archive', 'studio verify'],
  ['Finish a broken accept', 'studio recover NAME'],
  ['Play locally', 'studio serve'],
  ['Take a version from another branch', 'studio adopt NAME --from DIR'],
  ['Remove it from the page', 'studio unpublish NAME'],
  ['Render a stored version again', 'studio rerender NAME'],
  ['Rebuild the catalog', 'studio catalog'],
];

export function mountCommandSheet() {
  const open = document.getElementById('commands-open');
  const dialog = document.getElementById('command-sheet');
  if (!open || !dialog || dialog.dataset.ready) return;

  dialog.append(h('form', { method: 'dialog' },
    h('div', { class: 'command-sheet-bar' },
      h('h2', { id: 'command-sheet-title' }, 'Commands'),
      h('button', { class: 'command-sheet-close', value: 'close' }, 'Close')),
    h('p', {}, 'Create and revise pieces in Cursor. This page only plays them.'),
    h('p', {},
      'Say the same sentences in Cursor chat on the computer and at cursor.com/agents on the phone, on this repository. Begin with ',
      h('code', {}, 'Follow studio-tools/AGENT_INSTRUCTIONS.md'),
      ' unless that file is already the Cursor rule.'),
    ...SENTENCES.map(([label, sentence]) => h('blockquote', {},
      h('p', { class: 'command-label' }, label),
      h('p', { class: 'command-sentence' }, sentence))),
    h('p', {},
      'Create a composition named NAME runs ', h('code', {}, 'studio new NAME'),
      '. That command stops if the name already exists. Revise NAME runs ',
      h('code', {}, 'studio stage NAME'),
      '. That command stops if the name does not exist. The name is the context.'),
    h('p', {},
      'On the computer the agent pushes from the local checkout. On the phone the cloud agent clones the repository, runs the same commands, and pushes to master. This page plays the version after GitHub Pages rebuilds.'),
    h('h3', {}, 'Listen'),
    h('p', {},
      'This page. On the MacBook, ', h('code', {}, 'studio serve'),
      ', then 127.0.0.1:8000/studio/.'),
    h('h3', {}, 'Commands'),
    h('p', {},
      'From the repository root, with ', h('code', {}, 'studio-tools/bin'),
      ' on PATH. Replace NAME with the piece\'s name: ',
      h('code', {}, 'studio new MyName'), '.'),
    h('p', {},
      h('code', {}, 'studio new'), ' and ', h('code', {}, 'studio stage'),
      ' print a staging directory. Save it for the next commands: ',
      h('code', {}, 'S=$(studio new MyName)'),
      ', then ', h('code', {}, 'studio prompt "$S" --file request.txt'), '.'),
    h('div', { class: 'table-wrap' },
      h('table', { class: 'commands' },
        h('thead', {}, h('tr', {}, h('th', {}, 'You want'), h('th', {}, 'Command'))),
        h('tbody', {}, COMMANDS.map(([want, command]) =>
          h('tr', {}, h('td', {}, want), h('td', {}, h('code', {}, command))))))),
    h('p', { class: 'meta' },
      'accept refuses a staging area that has not rendered successfully. publish never writes soundfiles. discard does not touch the current version. studio COMMAND --help prints the flags. Useful flags: render --wav FILE (a soundfile outside the repository), publish --dry-run, recover --rollback.')));

  open.addEventListener('click', () => dialog.showModal());
  dialog.addEventListener('click', (event) => {
    if (event.target === dialog) dialog.close();
  });
  dialog.dataset.ready = '1';
}
