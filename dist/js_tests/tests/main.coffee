module = window ? this

Unitest = module.Unitest ||= {}

selfTests = []
testModules = []

Unitest.addTest = (name, callback) ->
  selfTests.push
    name: name
    callable: callback

passed = 0
failed = 0

Function::property = (prop, desc) ->
  Object.defineProperty @prototype, prop, desc

Unitest.addLog = (level, message...) ->
  if level == 'e'
    console.error message...
  else
    console.log message...

Unitest.log = (message...) -> Unitest.addLog('d', message...)

log = (args...) ->
  Chatle.log args...

print = (msg) ->
  container = document.querySelector("#main")
  container.innerHTML += "<p>" + msg + "</p>"

replace = (msg) ->
  container = document.querySelector("#main")
  container.removeChild(container.lastChild)
  container.innerHTML += "<p>" + msg + "</p>"

spin = ->
  spinner = document.querySelector(".spinner")
  a = ['-', '\\', '|', '/']
  if spinner
    idx = a.indexOf(spinner.innerHTML)
    nextSym = if (idx == (a.length - 1)) then 0 else idx + 1
    spinner.innerHTML = a[nextSym]
  setTimeout(spin, 50)

eq = (a, b) ->
  if a?.equals
    return a.equals(b)
  if b?.equals
    return b.equals(a)
  a == b


TestCase =
  assertEquals: (expected, found, msg) ->
    if !eq(expected, found)
      msg = "\n\tin: #{msg}" if msg
      throw "expected #{found} to be equal to #{expected}" + (msg || '')

  assertNull: (value) ->
    if value != null
      throw "expected #{value} to be null"

  assertNotNull: (value, text=null) ->
    if value == null
      throw text || "expected not null"

  assertTrue: (value, text) ->
    if value != true
      throw text || "expected #{value} to be true"

  assertTruthy: (value, text=null) ->
    if not value
      throw text || "expected #{value} to be truthy"

  assertFalse: (value) ->
    if value != false
      throw "expected #{value} to be false"

  assertFalsey: (value, text) ->
    if value
      throw text || "expected #{value} to be falsey"

  assertThrows: (klass, block) ->
    failure = null
    try
      await block()
      failure = "expected exception #{klass} but nothing was thrown"
    catch exception
      if klass && not(exception instanceof klass)
        failure = "expected exception #{klass} but #{exception} was thrown"
    throw failure if failure

  assert: (condition)->
    if !condition
      throw "assertion failed"
#  assertIncludes: (collection, item) ->
#    if collection.indexOf(item) < 0
#      throw "expected #{collection}\n to include #{item}"

Unitest.test = window.unitjs

Unitest.addTestModule = (module) ->
  testModules.push module

Unitest.runTests = ({except=null,only=null}={})->
  spin()
  m() for m in testModules
  new Promise (resolve, reject) ->
    testCase = TestCase

    for test in selfTests
      try
        continue if only && test.name != only
        if except && test.name.match(except)
          print "== skipping test:"+test.name
          continue
        print "-- test #{test.name}: running <span class='spinner'>|</span>"
        result = test.callable.apply(testCase)
        if result instanceof Promise
          await result
        replace "++ test #{test.name}: passed"
        passed++
      catch exception
        console.error exception
        console.error exception.stack if exception.stack
        replace "!! #{test.name} FAILED"
        failed++
    print "** Done executing ChatleUniversa tests, passed #{passed}, failed: #{failed}"
    print "** ChatleUniversa test is #{if failed > 0 then 'FAILED' else 'passed'}"
    if failed == 0
      resolve()
    else
      reject()

Unitest.addTest 'new testing system', ->
  @assertEquals 5, 2*2+1
