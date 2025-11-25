export function githubReadmeImpl({ token, user, repo, ref }) {
  return (onError, onSuccess) => {
    const query = `ref=${encodeURIComponent(ref)}`;
    const url = `https://api.github.com/repos/${user}/${repo}/readme?${query}`;

    fetch(url, {
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'application/vnd.github.v3+json',
        'X-GitHub-Api-Version': '2022-11-28',
      }
    }).then(async (response) => {
      if (!response.ok) {
        return onSuccess({
          name: '',
          content: '',
          ok: false,
          status: response.status,
        });
      }

      const data = await response.json();

      onSuccess({
        name: data.name,
        content: Buffer.from(data.content, 'base64').toString('utf-8'),
        ok: true,
        status: 200
      });
    }).catch((error) => {
      onError(error);
    });

    return (err, cancelError, cancelSuccess) => {
      cancelSuccess();
    };
  }
}
