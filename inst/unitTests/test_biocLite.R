test_reposType <- function()
{
    .httpRepos <- BiocInstaller:::.httpRepos
    .githubRepos <- BiocInstaller:::.githubRepos

    http <- c("http://foo.bar/baz", "https://foo.bar/baz")
    github <- c("foo/bar", "foo/bar@baz")
    neither <- c("foo", "foo/")
    all <- c(http, github, neither)

    checkIdentical(http, .httpRepos(all))
    checkIdentical(github, .githubRepos(all))
}
