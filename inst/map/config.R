app <- Builder$new(
  Static$new(
    urls=c('/javascript', '/images'),
    root='.'
  ),
  Static$new(
    urls='/json',
    root=tempdir()
  ),
  Brewery$new(
    url='/brew',
    root='.',
    jsonurl='../json/'
  ),
  Redirect$new('/brew/MapSites.rhtml')
)
