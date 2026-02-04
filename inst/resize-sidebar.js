// Resizable sidebar functionality
(function() {
  function initResizableSidebar() {
    const layout = document.querySelector('.bslib-sidebar-layout');
    const sidebar = layout?.querySelector('.sidebar');

    if (!layout || !sidebar) {
      // Retry if DOM not ready
      setTimeout(initResizableSidebar, 100);
      return;
    }

    // Check if handle already exists
    if (layout.querySelector('.sidebar-resize-handle')) {
      return;
    }

    // Create resize handle
    const handle = document.createElement('div');
    handle.className = 'sidebar-resize-handle';

    // Position handle at right edge of sidebar
    function positionHandle() {
      const sidebarRect = sidebar.getBoundingClientRect();
      const layoutRect = layout.getBoundingClientRect();
      handle.style.left = (sidebarRect.right - layoutRect.left - 3) + 'px';
    }

    layout.appendChild(handle);
    positionHandle();

    let isResizing = false;
    let startX, startWidth;

    handle.addEventListener('mousedown', function(e) {
      isResizing = true;
      startX = e.clientX;
      startWidth = sidebar.offsetWidth;
      handle.classList.add('resizing');
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
      e.preventDefault();
    });

    document.addEventListener('mousemove', function(e) {
      if (!isResizing) return;

      const diff = e.clientX - startX;
      const newWidth = startWidth + diff;
      const layoutWidth = layout.offsetWidth;

      // Constrain between min and max
      const minWidth = 200;
      const maxWidth = layoutWidth * 0.7;

      if (newWidth >= minWidth && newWidth <= maxWidth) {
        sidebar.style.width = newWidth + 'px';
        positionHandle();
      }
    });

    document.addEventListener('mouseup', function() {
      if (isResizing) {
        isResizing = false;
        handle.classList.remove('resizing');
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
      }
    });

    // Reposition handle on window resize
    window.addEventListener('resize', positionHandle);
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initResizableSidebar);
  } else {
    initResizableSidebar();
  }
})();
