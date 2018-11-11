document.onreadystatechange = function() {
  if (document.readyState === 'complete') {
    var main = document.getElementById('main');
    var controlUI = document.getElementById('control-ui');
    var modalRoot = document.getElementById('modal-root');

    // .control-block expand buttons
    var expandButtons = controlUI.querySelectorAll('.control-block > h2 > button');
    expandButtons.forEach(function(button) {
      button.addEventListener('click', function(e) {
        var controlBlock = e.currentTarget.parentNode.parentNode;
        if (/collapsed/.test(controlBlock.className) === false) {
          controlBlock.className = controlBlock.className + ' ' + 'collapsed';
        } else {
          controlBlock.className = controlBlock.className.replace(/\s?collapsed/, '');
        }
      });
    });

    // .tab-set Handlers
    var tabsetTabNavButtons = controlUI.querySelectorAll('.tabset .tab-nav button');
    tabsetTabNavButtons.forEach(function(navButton) {
      navButton.addEventListener('click', function(e) {
        var parentTabset = e.currentTarget.parentNode.parentNode;
        var tabNameRegex = new RegExp(e.currentTarget.getAttribute('data-tabname'));
        controlUI.querySelectorAll('.tab-nav button').forEach(function(tabButton) {
          if (tabButton.parentNode.parentNode === parentTabset) {
            tabButton.className = tabNameRegex.test(tabButton.getAttribute('data-tabname'))
              ? tabButton.className + ' active'
              : tabButton.className.replace(/active/, '');
          }
        });
        controlUI.querySelectorAll('.tab-content').forEach(function(tabContent) {
          if (tabContent.parentNode === parentTabset) {
            tabContent.className = tabNameRegex.test(tabContent.getAttribute('data-tabname'))
              ? tabContent.className + ' active'
              : tabContent.className.replace(/active/, '');
          }
        });
      });
    });

    // Modal Handlers
    var modalOpenButtons = main.querySelectorAll('.open-modal-button');
    var modalScreen = modalRoot.querySelector('.screen');
    var modalCloseButton = modalRoot.querySelector('.modal-header .modal-close');

    modalOpenButtons.forEach(function(button) {
      button.addEventListener('click', function(e) {
        modalRoot.className = 'open';
      });
    });

    modalScreen.addEventListener('click', function(e) {
      if (e.currentTarget === e.target) {
        modalRoot.className = '';
      }
    });

    modalCloseButton.addEventListener('click', function(e) {
      modalRoot.className = '';
    });
  }
};
