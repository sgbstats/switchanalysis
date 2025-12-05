function Image(el)
  -- Check if the path is relative (doesn't start with / or http)
  if not el.src:match("^/") and not el.src:match("^http") then
     -- Prepend the 'app/' folder to the image path
     el.src = "app/" .. el.src
  end
  return el
end