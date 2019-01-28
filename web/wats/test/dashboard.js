import test from 'ava';
import Fly from './helpers/fly'
import Web from './helpers/web'
import puppeteer from 'puppeteer';

const Suite = require('./helpers/suite');

const color = require('color');
const palette = require('./helpers/palette');

test.beforeEach(async t => {
  t.context = new Suite();
  await t.context.init(t);
});

test.afterEach(async t => {
  t.context.passed(t);
});

test.afterEach.always(async t => {
  await t.context.finish(t);
});

// some tests we'd like to see for dashboard autocomplete feature
// 1. focusing on text input opens dropdown with "status:" and "team:" options
// 2. "blurring" text input (as in hitting tab or clicking something other than a dropdown option or the X button) closes dropdown
// 3. clicking "status:" option in dropdown causes "status:" to be shown in the text field, the URL to show "?search=status:", changes the dropdown options to show the various statuses and keeps focus in the text field
// 3.a. clicking one of the statuses updates the text, URL and visible pipeline cards appropriately, closes the dropdown and keeps focus in the text field
// 4. clicking "team:" option in dropdown causes "team:" to be shown in the text field, the URL to show "?search=team:", changes the dropdown options to show the various teams and keeps focus in the text field
// 4.a. clicking one of the teams updates the text, URL and visible pipeline cards appropriately, closes the dropdown and keeps focus in the text field
//5. clicking X clears the text field, updates the URL to show no search, closes the dropdown and blurs the text input

async function showsPipelineState(t, setup, assertions) {
  await t.context.fly.run('set-pipeline -n -p some-pipeline -c fixtures/states-pipeline.yml');
  await t.context.fly.run('unpause-pipeline -p some-pipeline');

  await setup(t);

  await t.context.web.page.goto(t.context.web.route('/'));

  const group = `.dashboard-team-group[data-team-name="${t.context.teamName}"]`;
  await t.context.web.page.waitFor(`${group} .card`);
  const pipeline = await t.context.web.page.$(`${group} .card`);
  const text = await t.context.web.text(pipeline);

  const banner = await t.context.web.page.$(`${group} .banner`);
  const background = await t.context.web.computedStyle(banner, 'backgroundColor');

  await assertions(t, text, color(background), group);
};

test('does not show team name when unauthenticated and team has no exposed pipelines', async t => {
  t.context.web = await Web.build(t.context.url)
  await t.context.web.page.goto(t.context.web.route('/'));

  const group = `.dashboard-team-group[data-team-name="main"]`;
  const element = await t.context.web.page.$(group);

  t.falsy(element);
})

test('does not show team name when user is logged in another non-main team and has no exposed pipelines', async t => {
  await t.context.fly.run('set-pipeline -n -p some-pipeline -c fixtures/states-pipeline.yml');
  await t.context.fly.run('login -n ' + t.context.guestTeamName + ' -u '+ t.context.guestUsername +' -p ' + t.context.guestPassword);
  await t.context.fly.run('set-pipeline -n -p non-main-pipeline -c fixtures/states-pipeline.yml');

  let web = await Web.build(t.context.url, t.context.guestUsername, t.context.guestPassword);
  await web.login(t);
  await web.page.goto(web.route('/'));
  await web.page.waitFor('.dashboard-content', {timeout: 90000});
  const group = `.dashboard-team-group[data-team-name="${t.context.teamName}"]`;
  const element = await web.page.$(group);
  t.falsy(element);
})

test('shows pipelines in their correct order', async t => {
  let pipelineOrder = ['first', 'second', 'third', 'fourth', 'fifth'];

  for (var i = 0; i < pipelineOrder.length; i++) {
    let name = pipelineOrder[i];
    await t.context.fly.run(`set-pipeline -n -p ${name} -c fixtures/states-pipeline.yml`);
  }

  await t.context.web.page.goto(t.context.web.route('/'));

  const group = `.dashboard-team-group[data-team-name="${t.context.teamName}"]`;
  await t.context.web.page.waitFor(`${group} .pipeline-wrapper:nth-child(${pipelineOrder.length}) .card`);

  const names = await t.context.web.page.$$eval(`${group} .dashboard-pipeline-name`, nameElements => {
    var names = [];
    nameElements.forEach(e => names.push(e.innerText));
    return names;
  });

  t.deepEqual(names, pipelineOrder);
});

test.skip('shows pipelines with no finished builds in grey', showsPipelineState, async t => {
  // no setup
}, (t, text, background) => {
  t.regex(text, /some-pipeline/);
  t.regex(text, /pending/);

  t.deepEqual(background, palette.grey);
});

test.skip('shows paused pipelines in blue', showsPipelineState, async t => {
  await t.context.fly.run("pause-pipeline -p some-pipeline");
}, (t, text, background) => {
  t.regex(text, /some-pipeline/);
  t.regex(text, /paused/);

  t.deepEqual(background, palette.blue);
});

test.skip('shows pipelines with only passing builds in green', showsPipelineState, async t => {
  await t.context.fly.run("trigger-job -w -j some-pipeline/passing");
}, (t, text, background) => {
  t.regex(text, /some-pipeline/);
  t.deepEqual(background, palette.green);
});

test.skip('shows pipelines with any failed builds in red', showsPipelineState, async t => {
  await t.context.fly.run("trigger-job -w -j some-pipeline/passing");
  await t.throws(t.context.fly.run("trigger-job -w -j some-pipeline/failing"));
}, (t, text, background) => {
  t.regex(text, /some-pipeline/);
  t.deepEqual(background, palette.red);
});

test.skip('shows pipelines with any errored builds in amber', showsPipelineState, async t => {
  await t.context.fly.run("trigger-job -w -j some-pipeline/passing");
  await t.throws(t.context.fly.run("trigger-job -w -j some-pipeline/erroring"));
}, (t, text, background) => {
  t.regex(text, /some-pipeline/);
  t.deepEqual(background, palette.amber);
});

test.skip('shows pipelines with any aborted builds in brown', showsPipelineState, async t => {
  await t.context.fly.run("trigger-job -j some-pipeline/passing -w");

  let run = t.context.fly.spawn("trigger-job -j some-pipeline/running -w");

  run.childProcess.stdout.on('data', async data => {
    if (data.toString().indexOf("hello") !== -1) {
      await t.context.fly.run("abort-build -j some-pipeline/running -b 1");
    }
  });

  await t.throws(run);
}, (t, text, background) => {
  t.deepEqual(background, palette.brown);
});

test('auto-refreshes to reflect state changes', showsPipelineState, async t => {
  await t.context.fly.run("trigger-job -w -j some-pipeline/passing");
}, async (t, text, background, group) => {
  t.deepEqual(background, palette.green);

  await t.throws(t.context.fly.run("trigger-job -w -j some-pipeline/failing"));

  await t.context.web.page.waitFor(10000);

  let newBanner = await t.context.web.page.$(`${group} .banner`);
  let newBackground = await t.context.web.computedStyle(newBanner, 'backgroundColor');
  t.deepEqual(color(newBackground), palette.red);
});

test.skip('links to specific builds', async t => {
  await t.context.fly.run('set-pipeline -n -p some-pipeline -c fixtures/states-pipeline.yml');
  await t.context.fly.run('unpause-pipeline -p some-pipeline');
  await t.context.fly.run("trigger-job -w -j some-pipeline/passing");

  await t.context.web.page.goto(t.context.web.route('/'));

  const group = `.dashboard-team-group[data-team-name="${t.context.teamName}"]`;
  await t.context.web.clickAndWait(`${group} .node[data-tooltip="passing"] a`, '.build-header');
  t.regex(await t.context.web.text(), /passing #1/);
});
