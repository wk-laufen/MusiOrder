export const setupTimeoutNotification = progressElement => {
    const timeoutDelaySeconds = localStorage.getItem('timeout-notification-delay-seconds')
    if (timeoutDelaySeconds === null) {
        progressElement.style.display = 'none'
        return
    }

    const tickInterval = 0.1
    const timeoutDelayTicks = timeoutDelaySeconds / tickInterval
    progressElement.min = 0
    progressElement.max = timeoutDelayTicks
    let tickTimer = null
    
    const progressBarElement = progressElement.querySelector(':scope > div')
    let ticksLeft = timeoutDelayTicks

    const sendTimeout = () => {
        console.log('Sending timeout')
        window.parent.postMessage('inactivity-timeout', '*')
    }

    const updateDisplay = () => {
        progressBarElement.style.width = `${(timeoutDelayTicks - ticksLeft) / timeoutDelayTicks * 100}%`
    }

    const resetTimer = () => {
        clearInterval(tickTimer)
        ticksLeft = timeoutDelayTicks
        updateDisplay()

        tickTimer = setInterval(() => {
            ticksLeft--
            updateDisplay()
            if (ticksLeft <= 0) {
                clearInterval(tickTimer)
                sendTimeout()
            }
        }, tickInterval * 1000)
    }

    progressElement.addEventListener('click', sendTimeout)

    ;['touchstart', 'touchmove', 'click', 'keydown', 'scroll', 'mousemove'].forEach(ev =>
        document.addEventListener(ev, resetTimer, { passive: true })
    )

    resetTimer()
}
