glassdoor_api <- function(action = NULL,
                          other = NULL,
                          agent = "Mozilla/5.0",
                          version = 1,
                          format = "json",
                          url = glassdoor_url(),
                          pid = glassdoor_pid(),
                          pat = glassdoor_pat(),
                          ip_address = ipify::get_ip(),
                          ...
) {

  ua = httr::user_agent(agent)

  query = list(
    v = version,
    action = action,
    other = other,
    format = format,
    t.p = pid,
    t.k = pat,
    userip = ip_address,
    ...
  )

  resp <- httr::GET(url, query = query, ua)
  #
  # structure(list(
  #   content = parsed,
  #   path = path,
  #   response = resp
  # ),
  # class = "glassdoor_api")
}

res = glassdoor_api(
  action = "employer",
  other = NULL,
  version = 1,
  format = "json",
  q = "pharmaceuticals")