dec64 = Universa.utils.decode64
enc64 = Universa.utils.encode64

Unitest.addTestModule ->

  PACKED_PRIVATE_KEY_1 = dec64 'JgAcAQABvIEA/lcxRgJEfLoN0TqJkN6m/q+qiV3wGn3k53EEyIhVwqpWU9XntSCfPfIcTpfGrd2RvUZFEn1eW1Uc4XkDw5DZ++k2+lvbL5udllpbDtG9bSjG8Y0SruiwOUXijQzW/esWhi4b6OCCAcXCgIDDh5607JWAyS+XEK38V3MqfqfHbd+8gQDPVmtdpdownVM95HWwW/SLzy+XdHJA46pd8ROcnMnHJNjYREjFYeWzWSZJ4BviOXkdni82l8sH+szEcMeXT9m5Dl86L7vAkoUPT06OcUKc4aKB5ZtczMy2O4fCU+w1nmW+sEB/LgXleri2KBkDBwXEivvWXj2WCd8r4CRa+fhamQ=='
  PACKED_PRIVATE_KEY_2 = dec64 'JgAcAQABvIEA96FWTEq/Wjmd7kYbx04I/Ax3OQV+6e4YWu7xBr8k/SexvYvFEE3c9dRsZSsEj7KzYrNpIXezCsxO+j1sHADmeojwuveDdQQM6M6fbvygHq/mxKGllDoJTpzX/CnkuXDwC+lpRkkMTIF48GaYDM525951HYW1pAIYaVr+V5ctVQW8gQDGM71OC1yBLv6n23dEuu9Vqna2lvDpEWNO7MgKY0Ri9vUPwp+F+dUiKsRTbjnukDFqiYiNj+jkcWgiXqnjqdAGf9LUWHfF80W1PwUhkFw7torLJfaAr6bZ6FRzGcxTMad0x7Rz+5lkBjliKqOt8AUAIvMVe45G0c0StJSoqdqc4Q=='
  TEST_APP_TOKEN = "X19URVNUSU5HX1RPS0VOX2xramhsZGtqcWUhQCRsa2poYjMyMDk4NDFkdWIkOTE3LWghbGtqa2wsaTk="

  vassyaKey = dec64 '''
    JgAcAQABxAEBAP6P3NV8N/zxBKHMxrmOhDjnyZvMqygMF8xp205YFDK86uyBnPp83GgLWL8Y7mmfxqTPKdGqPj12oRrK17dnCY5NrFTBjJ2EUpwG0Cm7rVRKlWZPBvAcM/FnXdBQpdsH0mhmAsIv/l+t3WQaSu/N12xt0xKK5iTcQJLPf/1mxyiZaMPX6N4asM4vR2D0wtGudM7+ubSuaWkFFCwWNjZsP/xQY+QUx7qRB5WBnwR8TwzQzgDzwCdIBdyO7dtlfUy5zztsE3FJ0U46ykHkxAh0jv/Z0oA+i5pm0cOIocbmT0iwuY1sl5V7rKD8jfwfVqjiyxgEzbShijSlGLlYnU3wQSPEAQEA4Ni1uKZWdIlr3b2MeHCvCAYC5NL8DOjaVsvP2ewdeTXvksf6m9JUd0tirp7G7/RX7XzHOmYQ/avhcwS2213krfGg0026hWxsb025tnFbVwt0d8ZS3eSB+aqGc5FxuO+tp8Q9cjl2kfp1GXK7GCPTIbBM6vf/kZjN5szSua08qkMhXkt6LaEijCdrsG9IyRR+Jjl5JIWIgxBGH63wObJ6433R2ng1f+rmDO17tfoHNHhRQ5LixhhE97loFQLJXCbSMxxLSZnFdURSrfKMJdsUbZiQJOmZVjUGQwr5Cnj8hPhS59FKeUF0m+p6bYNwJzbgjOBGEIZ3g/+mZVlvVDwHvQ==
  '''
  sergeychKey = dec64 '''
    JgAcAQABxAEBAOXmhnvuxIn7o5jwvnRI+c6hm5rk+aVBjKp6ldFivLH+CPujiCxFUIJiEjHh21Nn66LXV+UoydqrWZx9uySyhbcyvU9LjKMxgDx8i6pSdB1+1Bd5SYUogqUpItVLuCeN8HZf7MPA4HQ1t9X0Umi5zUN3f2rpAC8xvKbhF3ZU/aPvluuBOT29UITbEMGpIM1u3AgwRCn68gCH+aG4Dlmo/ITzuAIn8VE9hMsuqowPVIEHA3qjw3PTqy9khTXualxVZhbJT00/VE43bSfLySBPM8srSOK7fmZNPSZzrDCMNKbSdn2KKjHIL25XE0wdZOLVcCg5kAtVUjmi0ZOylnRkiwfEAQEAxNYrc72lgLzr16lAU0O1MW6DtBqkhgrMM1wYUFzZyXIx8tr1P+zMGH6q6Lv5hazIrmTcywLnHGLPQL+gn671CT1uHwBNENIzgzXrZXX3GFv/dGQyPUb5WlVqTSTY++XK3kJtKP/UmAPbPr3OledbbjOj7cD8TcCM9AdIjY08ebyg/R1pNCAAq9JURPkYSZwRw8YKFQbhJFQshdLbtEdKSspKzjSvwbt8sjHqGRmOF2vhrvnicbvjp8BvoYnJZ/qHpbNWNK3CdrOBAe09djzZIoOILGPLh55y2fNp4C22jnbF1pdNo3fcxLW//A8v31bADjyW/8r7CZjJWjno6TxsYw==
  '''

  Unitest.addTest 'connectToPartyWithNick', ->
    api1 = await CryptoCloud.connect TEST_APP_TOKEN, PACKED_PRIVATE_KEY_1
    await api1.execute "test_destroy_party"
    api2 = await CryptoCloud.connect TEST_APP_TOKEN, PACKED_PRIVATE_KEY_2
    await api2.execute "test_destroy_party"
    api1 = await CryptoCloud.connect TEST_APP_TOKEN, PACKED_PRIVATE_KEY_1
    api2 = await CryptoCloud.connect TEST_APP_TOKEN, PACKED_PRIVATE_KEY_2

    @assertTrue await api1.registerNick('__nick_1__', true)
    @assertTrue await api2.registerNick('__nick_2__', false)

    # nick2 is not searchable
    @assertNull await api1.getParty(nick: '__nick_2__')

    # now we make it searchable
    @assertTrue await api2.registerNick('__nick_2__', true)

    # tries to connect to 2
    result = await api1.getParty(nick: '__nick_2__')
    @assertNotNull result

    @assertFalsey result.isAccepted, "is not yet accepted"
    @assertFalsey result.canBeAccepted, "can't be accepted by initiating party"

    @assertEquals api2.partyId, result.id
    # next call must return the same object!
    result2 = await api1.getParty(nick: '__nick_2__')
    @assertTrue result is result2, 'should be the same object on second call'
    k1 = result2.publicKey
    k2 = api2.privateKey.publicKey
    @assertEquals enc64(k1.packed), enc64(k2.packed)

    result = await api2.getParty(nick: '__nick_1__')
    @assertNotNull result

    @assertTrue result.isAccepted, "now is accepted"
    @assertFalsey result.canBeAccepted, "can't be accepted as is already accepted"
    @assertEquals api1.partyId, result.id

  Unitest.addTest 'message payload', ->
    vassya = await CryptoCloud.connect TEST_APP_TOKEN, vassyaKey
    sergeych = await CryptoCloud.connect TEST_APP_TOKEN, sergeychKey
    await vassya.execute "test_destroy_party"
    await sergeych.execute "test_destroy_party"

    vassya = await CryptoCloud.connect TEST_APP_TOKEN, vassyaKey
    sergeych = await CryptoCloud.connect TEST_APP_TOKEN, sergeychKey

    await vassya.registerNick('__vassya')
    await sergeych.registerNick('__sergeych')

    pser = await vassya.getParty nick: '__sergeych'

    src = "the ugly and stupid brown frog quickly jumped in the big shit"
    msg  = await pser.newMessage(src, {data: {value: 5}}).deliver()
    last = await sergeych.loadItems tags:['comm'], latestSerial: 10
    console.log(last[0])
    @assertEquals last[0].text, "the ugly and stupid brown frog quickly jumped in the big shit"

  Unitest.addTest 'interconnections', ->
    vassya = await CryptoCloud.connect TEST_APP_TOKEN, vassyaKey
    sergeych = await CryptoCloud.connect TEST_APP_TOKEN, sergeychKey
    await vassya.execute "test_destroy_party"
    await sergeych.execute "test_destroy_party"

    vassya = await CryptoCloud.connect TEST_APP_TOKEN, vassyaKey
    sergeych = await CryptoCloud.connect TEST_APP_TOKEN, sergeychKey

    await vassya.registerNick('__vassya')
    await sergeych.registerNick('__sergeych')

    pser = await vassya.getParty nick: '__sergeych'

    src = "the ugly and stupid brown frog quickly jumped in the big shit"
    msg  = await pser.newMessage(src).deliver()
    last = await sergeych.loadItems tags:['comm'], latestSerial: 10

    console.log("LAST", last)
    console.log (rm=last[0]).toString()

    @assertFalse rm.from.isAccepted
    @assertTrue rm.from.canBeAccepted
    @assertFalse msg.to.isAccepted

    await rm.from.accept()

    @assertTrue rm.from.isAccepted
    @assertFalsey rm.from.canBeAccepted

    await vassya.reloadParties()

    @assertTrue msg.to.isAccepted
    @assertFalsey msg.to.canBeAccepted

    @assertEquals msg.to, pser
    @assertEquals msg.from.id, vassya.me.id

    @assertEquals msg.text, rm.text
    @assertEquals msg.id, rm.id

    @assertEquals rm.from.id, vassya.me.id
    @assertEquals rm.to.id, sergeych.me.id
    @assertEquals 'p2pchat', rm.type

    last = await vassya.loadItems tags:['comm'], latestSerial: 10
    console.log (rm=last[0]).toString()
    @assertEquals rm.from.id, vassya.me.id
    @assertEquals rm.to.id, sergeych.me.id, "downloaded message from me, to"
    @assertEquals 'p2pchat', rm.type
